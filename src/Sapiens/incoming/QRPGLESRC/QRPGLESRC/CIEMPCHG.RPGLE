     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H DFTACTGRP(*no) ACTGRP(*caller)
     H BNDDIR('WTBNDDIR':'YAJL')
    H*********************************************************************
    H/TITLE CIEMPCHG - Transfer Employer Changes to CiCenter
    H*********************************************************************
    H* Program desription:                                               *
    H*  This program will gather information from the employer and create*
    H*   and XML script to be used by CiCenter.  This script will be     *
    H*   transfered to a data queue for the CiCenter to retrieve and     *
    H*   process.                                                        *
    H*                                                                   *
    H* Use the following from a command line to clear the data queue:    *
    H*  CALL PGM(QCLRDTAQ) PARM(CIEMPDTAQ *LIBL)                         *
    H* This should only be used during testing.                          *
    H*                                                                   *
    H* Created: 05/23/08   C:33    Project: 132DKH                       *
    H*********************************************************************
    H* Revisions:                                                        *
     H*                                                                   *
     H* 01/24/19  C:33  Project: SY406                                    *
     H*                 Include Portal with change information            *
     H*                                                                   *
     H* 09/12/14 C:295 Project: CI103                                     *
     H*                To change the XML Schema for Employer to make it   *
     H*                different from Agent as the same data queue is used*
     H*                for both .                                         *
     H*                                                                   *
     H* 07/14/14 C:295 Project: CI103                                     *
     H*                Update Agent details from WCMS to FQCIC            *
     H*                Changing the Name of data queue CIEMPCHG instead of*
     H*                CIEMPDTAQ.                                         *
     H*                                                                   *
     H* 08/28/13 C:287 Project: CI126                                     *
     H*                Include the Division# for the Name/FEIN changes.   *
     H*                Added %Parm to determine if parameter Div was incld*
     H* 09/11/13 C:287 Change position of Division in HTML section, also  *
     H*                change company name from WCMS to CompSuite.        *
    H*                                                                   *
    H* 03/21/12 C:136 Support: WA045 79515                               *
    H*                Do not remove leading zeroes from FEIN             *
    H*                                                                   *
     H* 07/25/11  C:33   Seq#: WA139/74434                                *
     H*                  Adapt for exception characters.                  *
    H*                                                                   *
     H* 09/21/10  C:33   Project: 71DKX  Priss #9                         *
     H*                  Do not run in quote environment.                 *
    H*                                                                   *
     H* 08/10/10  C:33   Project: 71DKX                                   *
     H*                  Use data area to determine if job should run.    *
    H*                                                                   *
     H* 01/28/10  C:XXX  Project: SY215                                   *
     H*                  Retrieve brand Name                              *
     H*                                                                   *
    H* 12/19/09  C:33  Project: 139DNT                                   *
    H*                 Include '139' also.                               *
    H*********************************************************************
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fswcpf     if   e             disk
     Fssylf     if   e           k Disk    ReName(ssy01:psy00)
     F                                     Ignore(spc01)
     F                                     Ignore(swc01)
     F                                     Ignore(sfb01)
     F                                     Ignore(src01)
     F                                     Ignore(sdr01)
     F                                     Ignore(sbc01)
     F                                     Ignore(scs01)
     F                                     Ignore(sgl01)
     F                                     Ignore(sp201)

     D*entry
     DCIEMPCHG         pr                  extpgm('CIEMPCHG')
     D @@RRN                          9  0
     D @@Type                        10
     D @@OldFein                      9  0
     D @@OldName                    100
     D @@Division                     5  0 const options(*nopass)
     DCIEMPCHG         pi
     D @@RRN                          9  0
     D @@Type                        10
     D @@OldFein                      9  0
     D @@OldName                    100
     D @@Division                     5  0 const options(*nopass)

     DLoadDataQ        pr                  extpgm('QSNDDTAQ')
     D ##DataQ                       10    const
     D ##DataQLib                    10    const
     D ##DataQLen                     5  0 const
     D ##Data                      2500    const
     D*
     DCheckLib         pr                  extpgm('SRCHKLIB')
     D ##Lib                         10    const
     D ##Type                         4    const
     D ##AppendSfx                    1    const
     D ##Flag                         1
     D*
     DRmvXMLExcptChar  pr         32766    opdesc
     D ##inField                  32766    options(*varsize) const
     D ##incSize                      5  0 options(*nopass)
     D*
     DCEEDOD           pr
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)

     D ChgUser         pr           240
     D ChgDate         pr           240
     D DbaChgFtr       pr           240
     D DbaChgHdr       pr           540
     D NewDBA          pr           240
     D NewFEIN         pr           240
     D OldDBA          pr           240
     D OldFEIN         pr           240
     D StandardHdr     pr           120
     D Division        pr            60

     D#ofExcept        c                   5
     D excChar         s              1    dim(#ofExcept) ctdata perrcd(1)
     D replace         s             10    dim(#ofExcept) alt(excChar)
     D*
     D* parameters passed to CEEDOD
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     C*
     D upSize          s              5  0
     D*
     D cltID           s              3
     D data            s           2500
     D***  Begin Add  ***  01/24/19  **************************************
     D isUseCiCenter   s              1n   inz(*off)
     D isUsePortal     s              1n   inz(*off)
     D***  End   Add  ***  01/24/19  **************************************
     D process         s              1n   inz(*off)
     D quoteEnv        s              1n   inz(*off)
     D chardtaq        s                   Like(SYfl14)
     D***  Begin Add  ***  01/24/19  **************************************
     D strJson         s           5000a   varying

     D CRLF            C                   x'0D25'

     D/copy sPrDate
     D/copy sPrJson
     D/copy sPrModule

      /Include yajl_h
     D***  End   Add  ***  01/24/19  **************************************

     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C*
     C                   callp(e)  CheckLib('PDSQTE':'*USR':'1':quoteEnv)
     C*
     C     *dtaara       define    ciEmpChg      process
     C                   in        process                              99
     C                   select
     C                   when      quoteEnv
     C                   eval      process = *off
     C                   when      cltID = '132'
     C                              or cltID = '139'
     C                   eval      process = *on
     C                   other
     C                   endsl
     C*
       //***  Begin Chg  ***  01/24/19  ***********************************
     C**********         clear                   WCFL19
     C*****1****         setll     swcpf
     C**********         read      swcpf
     C**If*CiCenter is not installed, don't process
     C**********         if        WCFL19 <> 'Y'
      /Free
       isUseCiCenter = IsModEnabled('CiCenter');
       isUsePortal = IsModEnabled('Portal Module');

       If not isUseCiCenter and
        not isUsePortal;
      /End-free
       //***  End   Chg  ***  01/24/19  ***********************************
     C                   eval      process = *off
     C                   endif
     C*
     C                   if        not process
     C                   eval      *inlr = *on
     C                   return
     C                   endif

     C* If Employer record is not found, end program
     C     @@RRN         chain     wmemp
     C                   if        not %found
     C                   eval      *inlr = *on
     C                   return
     C                   endif

     C                   select
     C                   when      @@Type = 'FEIN'
     C                   exsr      CrtFeinXML
     C                   when      @@Type = 'DBA'
     C                   exsr      CrtDbaXML
     C                   endsl

      /Free

       //***  Begin Add  ***  01/24/19  ********************************
          Exsr CreateJson;
       //***  End   Add  ***  01/24/19  ********************************

       // Retrieve data queue name
       Chain(n) 'SY' psy00;
       If %Found;
           Chardtaq = SYfl14;
       Endif;

       //***  Begin Chg  ***  01/24/19  ***********************************
       //****callp LoadDataQ (Chardtaq:'*LIBL':%len(%trim(data)):data);
       If isUseCiCenter;
          callp LoadDataQ (Chardtaq:'*LIBL':%len(%trim(data)):data);
       Endif;
       //***  End   Chg  ***  01/24/19  ***********************************

      /End-Free
     C                   eval      *inlr = *on

       //***  Begin Add  ***  01/24/19  ********************************
       //==================================================================
       // Create the Json for the changes
       //==================================================================
      /Free
       Begsr CreateJson;
        //strJson += 'Content-Type: text/plain' + CRLF + CRLF;
        strJson += '{"employerChange":{' +
         '"name":{' +
          BuildJsonNode('old':%trim(@@OldName)) + ',' +
          BuildJsonNode('new':%trim(EMname)) +
          '},' +
         '"fein":{' +
          BuildJsonNode('old':%editc(@@OldFein:'X')) + ',' +
          BuildJsonNode('new':%editc(EMfein:'X')) +
         '},';
        If %parms >= 5;
           strJson +=
            BuildJsonNode('division':%editc(@@division:'X')) + ',';
        Endif;
        strJson +=
         '"changeInfo":{' +
          BuildJsonNode('user':%trim(EMuser)) + ',' +
          BuildJsonNode('date':%char(DtMDYY(EMdt)):'date') +
         '}' +
         '}}';
       Endsr;
      /End-free
       //***  End   Add  ***  01/24/19  ********************************

     C*====================================================================
     C* Create the XML for employer DBA name changes
     C*====================================================================
     C     CrtDbaXML     begsr
     C                   If        %Parms >= 5
     C*
     C* This first 'data' is used for debugging.  Using the DSPDTAQ command
     C*  each line will appear evenly in the data queue.  Size of each
     C*  field used is divisable by 60 for this reason.
     C                   eval      data = StandardHdr
     C                                  + DbaChgHdr
     C                                  + OldDBA
     C                                  + OldFEIN
     C                                  + NewDBA
     C                                  + NewFEIN
     C                                  + ChgUser
     C                                  + ChgDate
     C                                  + Division                              **Added 09/12/14
     C                                  + DbaChgFtr
     C*
     C                   eval      data = %trim(StandardHdr) + ' '
     C                                  + %trim(DbaChgHdr)   + ' '
     C                                  + %trim(OldDBA)      + ' '
     C                                  + %trim(OldFEIN)     + ' '
     C                                  + %trim(NewDBA)      + ' '
     C                                  + %trim(NewFEIN)     + ' '
     C                                  + %trim(ChgUser)     + ' '
     C                                  + %trim(ChgDate)     + ' '
     C                                  + %trim(Division)    + ' '              **Added 09/12/14
     C                                  + %trim(DbaChgFtr)   + ' '
     C                   Else
     C*
     C*
     C* This first 'data' is used for debugging.  Using the DSPDTAQ command
     C*  each line will appear evenly in the data queue.  Size of each
     C*  field used is divisable by 60 for this reason.
     C                   eval      data = StandardHdr
     C                                  + DbaChgHdr
     C                                  + OldDBA
     C                                  + OldFEIN
     C                                  + NewDBA
     C                                  + NewFEIN
     C                                  + ChgUser
     C                                  + ChgDate
     C                                  + Division                              **Added 09/12/14
     C                                  + DbaChgFtr
     C*
     C                   eval      data = %trim(StandardHdr) + ' '
     C                                  + %trim(DbaChgHdr)   + ' '
     C                                  + %trim(OldDBA)      + ' '
     C                                  + %trim(OldFEIN)     + ' '
     C                                  + %trim(NewDBA)      + ' '
     C                                  + %trim(NewFEIN)     + ' '
     C                                  + %trim(ChgUser)     + ' '
     C                                  + %trim(ChgDate)     + ' '
     C                                  + %trim(Division)    + ' '              **Added 09/12/14
     C                                  + %trim(DbaChgFtr)   + ' '
     C                   EndIf
     C*
     C                   endsr

     C*====================================================================
     C* Create the XML for employer FEIN changes
     C*====================================================================
     C     CrtFeinXML    begsr
     C* The FEIN and DBA XML scripts are currently the same, call DBA script
     C                   exsr      CrtDbaXML
     C                   endsr





     P*====================================================================
     P* Create Requsting User XML
     P*====================================================================
     PChgUser          b
     D ChgUser         pi           240

     D data            s            240

      /free
       data = '<requestedBy>'
            + %trim(RmvXMLExcptChar(EMUSER))
            + '</requestedBy>';
       return data;
      /end-free

     PChgUser          e





     P*====================================================================
     P* Create Requsted Date XML
     P*====================================================================
     PChgDate          b
     D ChgDate         pi           240

     D data            s            240
     D date            s               d   datfmt(*iso)

     C     *cymd         test(d)                 EMDT                   99
     C  n99*cymd         move      EMDT          date
     C                   eval      data = '<requestedDate>'
     C                                  + %char(date)
     C                                  + '</requestedDate>'
     C                   return    data

     PChgDate          e





     P*====================================================================
     P* Create DBA change XML footer
     P*====================================================================
     PDbaChgFtr        b
     D DbaChgFtr       pi           240

     D data            s            240

     C                   eval      data = '</InsuredChangeRequest> '
     C                             + '</CompSuiteCompanyChangeRequest>'
     C                   return    data

     PDbaChgFtr        e





     P*====================================================================
     P* Create DBA change XML header
     P*====================================================================
     PDbaChgHdr        b
     D DbaChgHdr       pi           540

     D data            s            540

     D xsi             C                   'http://www.w3.org/2001-
     D                                     /XMLSchema-instance'
     D xmlns           C                   'http://www.stoneriver.com/comp-
     D                                     suite/namespace'
     D schemaLocation  C                   'CompSuiteCompanyChangeRequest.xsd'

     C                   eval      data =
     C                               '<CompSuiteCompanyChangeRequest '
     C                                  + ' xsi:noNamespaceSchemaLocation="'
     C                                  + schemaLocation + '"'
     C                                  +   ' xmlns:xsi="' + xsi + '"' + '> '
     C                                  + '<InsuredChangeRequest>'
     C                   return    data

     PDbaChgHdr        e





     P*====================================================================
     P* Create new DBA XML
     P*====================================================================
     PNewDBA           b
     D NewDBA          pi           240

     D data            s            240

      /free
       data = '<newCompanyName>'
            + %trim(%subst(RmvXMLExcptChar(EMNAME:upSize)
                          :1:%size(EMNAME)+upSize)
                  + RmvXMLExcptChar(EMNAM2))
            + '</newCompanyName>';
       return data;
      /end-free

     PNewDBA           e





     P*====================================================================
     P* Create new FEIN XML header
     P*====================================================================
     PNewFEIN          b
     D NewFEIN         pi           240

     D data            s            240

      /free
       data = '<newFEIN>'
            + %trim(RmvXMLExcptChar(%editc(EMFEIN : 'X')))
            + '</newFEIN>';
       return data;
      /end-free

     PNewFEIN          e





     P*====================================================================
     P* Create old DBA XML
     P*====================================================================
     POldDBA           b
     D OldDBA          pi           240

     D data            s            240
     D name            s                   like(@@OldName)

      /free
       name = @@OldName;
       data = '<oldCompanyName>'
            + %trim(RmvXMLExcptChar(name) + ' ')
            + '</oldCompanyName>';
       return data;
      /end-free

     POldDBA           e





     P*====================================================================
     P* Create old FEIN XML header
     P*====================================================================
     POldFEIN          b
     D OldFEIN         pi           240

     D data            s            240

      /free
       data = '<oldFEIN>'
            + %trim(RmvXMLExcptChar(%editc(@@OldFEIN : 'X')))
            + '</oldFEIN>';
       return data;
      /end-free

     POldFEIN          e





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
       callp CEEDOD(1:descType:dataType:descInfo1:descInfo2:inLen:*omit);
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
     P* Create standard XML header
     P*====================================================================
     PStandardHdr      b
     D StandardHdr     pi           120

     D data            s            120

     C                   eval      data = '<?xml version="1.0"'
     C                                  + ' encoding="UTF-8" ?>'
     C                   return    data

     PStandardHdr      e


     P*===================================================================
     P* Added - 08/28/13  *************************************************
     P*Create Division Code XML element
     P*===================================================================
     PDivision         b
     D Division        pi            60

     D data            s             60

      /Free
       data = '<divisionCode>'
            + %trim(RmvXMLExcptChar(%editc(@@Division : 'X')))
            + '</divisionCode>';
       Return data;
      /End-Free

     PDivision         e

** Exception Characters & Replacements         07/25/11
&&amp;                    <<<==== MUST BE FIRST
<&lt;
>&gt;
'&apos;
"&quot;
