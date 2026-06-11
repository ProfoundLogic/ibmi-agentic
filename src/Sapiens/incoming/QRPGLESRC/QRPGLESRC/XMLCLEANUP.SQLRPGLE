     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) bnddir('STBNDDIR')
     H*====================================================================
     H/TITLE XMLCLEANUP - Weekly program to clean up xml files.
     H*====================================================================
     H*  Description:
     H*
     H*  This program can be called to clean up the XML files and the
     H*  XML preparation data file.
     H*
     H*  Created:  04/03/15  C:65  Project: SY251
     H*====================================================================
     H*  Revisions:
     H*
     H*  08/25/20 C:65  Project: Phw#91006 (Sunz)
     H*                 Perform RGZPFM on WXWXFMP to ensure it does not
     H*                 degrade preformance for the java server app.
     H*
     H*  01/20/17 C:65  Project: SY251
     H*                 Fix # of days passed to cleanifs.
     H*
     H*  06/06/16 C:33  Project: SY271
     H*                -Rewrite. Genericize.
     H*
     H*  04/29/16 C:65  Project: SY251
     H*                 Clean up documents in archive folder.  Remove
     H*                 documents in main folder based on 0 days since
     H*                 they are unsecured (and reside from cancelled
     H*                 interactive documents).
     H*
     H*====================================================================
     FSTSCP     IF   E             DISK    rename(psc01:psc0p)
     FSTSFP     IF   E             DISK    rename(psf01:psf0p)
     D
     DcompareDt        s               d
     DsqlStmt          s            100
     D*** begin add ***  01/20/17  **********************************
     DintDays          s              4  0
     D*** end add *****  01/20/17  **********************************
     d*** begin add ***  08/25/20  ************************************
     d strCommand      s            255

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     d*** end add *****  08/25/20  ************************************

     DCleanIFS         pr                  extPgm('CLEANIFS')
     D outDir                       100    const
     D outDays                        4    const
     C*
      /Free
       Read psc0p;

       // Clean up the XML IFS files (0 days).
       CleanIFS(%trim(SCXMLF):'0000');

       // If clean up days is not set, default to 7
       If SC1102 = 0;
          SC1102 = 7;
       Endif;
       //*** begin add ***  01/20/17  **********************************
       intDays = SC1102;
       //*** end add *****  01/20/17  **********************************

       // Clean up the XML IFS archive files (after x days)
       //*** begin chg ***  01/20/17  **********************************
       //*****CleanIFS(%trim(SCXMLA):%editc(SC1102:'X'));
       CleanIFS(%trim(SCXMLA):%editc(intDays:'X'));
       //*** end chg *****  01/20/17  **********************************

       // Clean up the XML Prep files (after x days)
       compareDt = %date - %days(SC1102);
       Dou %eof(stsfp);
          Read psf0p;
          If not %eof;
             sqlStmt = 'Delete from ' + %trim(SFfile) +
              ' where ' + %trim(SFdtFld) + ' < ' + %char(compareDt:*CYMD0) +
              ' with NC';
             Exec Sql Execute IMMEDIATE :sqlStmt;
          Endif;
       Enddo;

       //*** begin add ***  08/25/20  ************************************
          Monitor;
             Clear strCommand;
             strCommand = 'RGZPFM FILE(WXWXFMP)';
             Callp QCMDEXC(strCommand:%len(strCommand));
          On-error;
          Endmon;
       //*** end add *****  08/25/20  ************************************

       *inlr = *on;
      /end-free
