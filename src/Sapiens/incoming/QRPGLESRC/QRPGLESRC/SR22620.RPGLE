     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')
      /TITLE SR22620 - MN Entity Address Schedule Attach/Print Pgm
      //===============================================================
      // WC220620 - Minnesota Entity Address Schedule Endorsement
      //
      // PROGRAM DESCRIPTION:
      //
      //   Determines if this endorsement form should be attached to
      //   the policy and creates data in the Endorsement Form
      //   Workfile (WXFMP) prior to a form merge.
      //===============================================================
      // Revisions:
      //
      //===============================================================
      // NOTES:
      //
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).
      //
      //  qqmode = 1 - Called to determine if form should be attached.
      //  qqmode = 2 - Called to print endorsement.
      //  qqmode = 3 - Called for additional input.
      //  qqmode = 4 - Called for midterm processing.
      //===============================================================
     fsxdop     if   e           k disk    rename(pdo01:pdo00)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwxpwp     if a e             disk    rename(ppw01:ppw0p)
     fwxp2p     if a e             disk    rename(pp201:pp20p)
     fxmlprel3  if   e           k disk    rename(xp100:xp103)
     fxmlteml   if   e           k disk    rename(xt100:xt10l)
      /copy sCopyEn01F
      /copy sCopyEn01D
     d qqForm          s             15A   Inz('WC220620  ')
     d svXmlt          s                   like(XTxmlt)
     d xxZip4          s              4A
     d addpgm          c                   const('SRXXXXXXXX')
       //*Entry
     d sr22620         pr
     d                                3
     d                                3
     d                                9
     d                                5
     d                                3
     d                                1
     d                                9  0
     d                                1
     d sr22620         pi
     d  qqCo#                         3
     d  qqFnd                         3
     d  qqEmp#                        9
     d  qqDiv                         5
     d  qqFyr                         3
     d  qqMode                        1
     d  qqRrn                         9  0
     d  qqMod2                        1
      /copy sCopyEn01C
      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr OthInf;
       Exsr WriteWorkFile;
       // Retrieve endorsement template name
       Chain ldaXml xp103;
       If Not(%Found);
          LeaveSr;
       Endif;
       // Find policy page work file record with policy template name
       Setgt *Hival ppw0p;
       Dou %Eof;
          Readp ppw0p;
          If %Eof;
              LeaveSr;
          Endif;
          If PWxml = XPXmlT;
             // Write pwp0p with template file name of endorsement
             PWxml = inXml;
             Write ppw0p;
             // Write p2p0p with p2key = 0 and p2xml = inXml for
             //  policy name/address
             Chain (PWco#:PWfnd:PWemp#:PWdiv) pem01;
             Clear pp20p;
             P2xml = inXml;
             P2key = 0;
             P2type = 'L';
             P2LocName = %Trim(PWPolName);
             P2LocFdnm = %Trim(PWPolFdNm);
             P2fein = PWfein;
             P2EffDate = PWFrmDate;
             P2ExpDate = PWToDate;
             P2MailAd1 = %trim(PWPolAdr1);
             P2MailAd2 = %trim(PWPolAdr2);
             P2MailCty = %trim(PWPolCity);
             P2MailSt  = %trim(PWPolSt);
             P2MailZip = PWPolZip;
             P2MailCsz = %trim(PWPolCity) + ', ' + PWPolSt + '  ' +
              %Subst(%Editc(PWPolZip:'X'):1:5);
             xxZip4 = %Subst(%Editc(PWPolZip:'X'):6:4);
             If xxZip4 <> '0000';
                P2MailCsz = %trim(P2MailCsz) +  '-' + xxZip4;
             Endif;
             P2StUE#   = %Trim(EmUe#);
             P2TaxID   = %Trim(EmRsk#);
             P2RiskID  = %trim(EmSemp);
             P2NAICS   = %trim(EmNaic);
             P2SIC     = %trim(EmSic);
             P2LeaseID = %trim(EMLsId);
             Chain (EMco#:'NREL':EMlsid) pgt00;
             P2LeaseDs = GTdesc;
             P2Div#    = EMdiv;
             P2Pgm     = Q1pgm;
             P2User    = Q1user;
             P2Dt   = %Dec(%Date():*cymd);
             P2time = %Dec(%Time():*hms);
             Write pp20p;
             Leave;
          Endif;
       Enddo;
       // Read through xttemp by ldaXml(output file) and find
       // 'Schedule of Workplaces' to get template name.
       // If not found - leaveSr
       // If found, readp on WXP2P and select 'L' type records
       // in which p2xml = template name.  Change p2xml to inXml and
       // write new record.
       Setll ldaXml xt10l;
       Dou %Eof;
          Reade ldaXml xt10l;
          If %Eof;
             LeaveSr;
          Endif;
          If XTtype = 'Schedule of Workplaces';
             Setgt *Hival pp20p;
             Dou %Eof;
                Readp pp20p;
                If %Eof;
                   Leave;
                Endif;
                If svXmlT <> *Blanks and svXmlT <> P2xml;
                   Leave;
                Endif;
                If P2type = 'L' and P2xml = XTxmlt;
                   svXmlT = XTxmlt;
                   P2xml = inXml;
                   Write pp20p;
                Endif;
             Enddo;
             Leave;
          Endif;
       Enddo;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr Pgmstr;
       writeInCopyBook = *Off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr Pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
       crtFrm = *On;
       Endsr;
      /End-Free
