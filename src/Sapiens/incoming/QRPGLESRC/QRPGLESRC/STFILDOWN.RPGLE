     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('STBNDDIR')
     H/TITLE STFILDOWN - Download file from server
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Download file from server               *
     H*                                                               *
     H* CALLED BY: MENU                                               *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED: 04/01/13  C:191 PROJECT: Transform                   *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 07/13/18  C:33  Project: 150WAD  71517                        *
     H*                 The prior change only changed the order of    *
     H*                  selection.                                   *
     H*                                                               *
     H* 03/07/18  C:33  Project: 150WAD                               *
     H*                 Default output type to .TXT for CLT150        *
     H*                                                               *
     H* 06/03/16  C:33  PHW32782                                      *
     H*                 Add a root folder.                            *
     H*                                                               *
     H* 01/27/15  C:33  PHW32672                                      *
     H*                 Add folder prefix.                            *
     H*                                                               *
     H* 05/27/14  C:290 Support: 85690                                *
     H*                 Changed q1jobn defination in display file as  *
     H*                 "Number" with zero fill. So that AJAX call    *
     H*                 from this program does not fail.(display file *
     H*                 changes only).                                *
     H*                                                               *
     H* 09/26/13  C:33  Redmine: 2817                                 *
     H*                 Allow for stored procedure to be in different *
     H*                  library in each environment.                 *
     H*****************************************************************
     Fstfildown cf   e             workstn
     F                                     handler('PROFOUNDUI(HANDLER)')
     D psds          esds                  extname(szq1p)
     D*
     DiURL             s            256    inz
     D*
     D/copy sPrPrprty
     D***  Begin Add  ***  06/03/16  **************************************
     D/copy sPrRunCmd
     D***  End   Add  ***  06/03/16  **************************************
     D*
     DRtvObjLib        pr                  ExtPgm('RTVOBJLIB')
     D inObj                         10    const
     D inObjType                      7    const
     D outLib                        10

     D***  Begin Add  06/03/16  *******************************************
     DCrtDftDir        pr
     D***  End   Add  06/03/16  *******************************************
     D*
     DGetObjLib        pr            10
     D inObj                         10    const
     D inObjType                      7    const
     D*
     D***  Begin Add  ***  03/07/18  **************************************
     d RtvCltId        pr                  ExtPgm('RTVCLTID')
     d  cltId                         3
     D***  End   Add  ***  03/07/18  **************************************
     D*
     DRtvFlrPrefix     pr                  ExtPgm('SA039R')
     D                               60
     DGetDirectory     pr                  like(scDir)

     D***  Begin Add  ***  03/07/18  **************************************
     Di1               s              3  0
     D#ofOutCh         c                   10
     DdsOutCh          s             50    dim(#ofOutCh) ctdata perrcd(1)
     DdsOutChVal       s              5    dim(#ofOutCh) alt(dsOutCh)
     dcltId            s              3
     D***  End   Add  ***  03/07/18  **************************************
     D***  Begin Add  ***  06/03/16  **************************************
     DdftDir           c                   '/FileDownloads/'
     D***  End   Add  ***  06/03/16  **************************************
     D*
      /Free
       //***  Begin Add  ***  03/07/18  ***********************************
         RtvCltId(cltId);
         Clear scOutCh;
         Clear scOutChVal;
         //***  Begin Add  ***  07/13/18  ********************************
         Clear scType;
         //***  End   Add  ***  07/13/18  ********************************
         Select;
         When cltId = '150';
          //***  Begin Chg  ***  07/13/18  ****************************
          //scOutCh = dsOutCh(4);
          //scOutChVal = dsOutChVal(4);
            scOutCh = ',' + dsOutCh(4);
            scOutChVal = ',' + dsOutChVal(4);
            scType = dsOutChVal(4);
          //***  End   Chg  ***  07/13/18  ****************************
            Clear dsOutCh(4);
            Clear dsOutChVal(4);
         Endsl;
         For i1 = 1 to #ofOutCh;
            If dsOutCh(i1) = *blanks;
               Iter;
            Endif;
            If scOutCh <> *blank;
               scOutCh = %trim(scOutCh) + ',';
               scOutChVal = %trim(scOutChVal) + ',';
         //***  Begin Add  ***  07/13/18  ********************************
            Else;
               scOutCh = ',';
               scOutChVal = ',';
         //***  End   Add  ***  07/13/18  ********************************
            Endif;
            scOutCh = %trim(scOutCh) + dsOutCh(i1);
            scOutChVal = %trim(scOutChVal) + dsOutChVal(i1);
         Endfor;
       //***  End   Add  ***  03/07/18  ***********************************
         *inlr = *on;
         dou scStPrLib <> ' ';
         iURL = %Trim(SysProperty('iAccessURL'));
         scIAccess = %Trim(iURL)+'/'+%Trim(SysProperty('iAccessDBExec'))+'/';
         scConnect = %Trim(SysProperty('iAccessDBConn'));
         scStPrLib = GetObjLib('GET_M00001':'*PGM');
         scDir = GetDirectory;
         Exfmt rcdDown;
         Enddo;
      /End-Free

     P***  Begin Add  06/03/16  *******************************************
     PCrtDftDir        b
     DCrtDftDir        pi

     Dcmd              s            100

      /Free
       Monitor;
       cmd = 'CRTDIR DIR(''' + dftDir + ''') DTAAUT(*INDIR) OBJAUT(*INDIR)';
       RunCmd(cmd:%len(cmd));
       On-Error;
       Endmon;
      /End-free

     PCrtDftDir        e
     P***  End   Add  06/03/16  *******************************************

     PGetObjLib        b
     DGetObjLib        pi            10
     D inObj                         10    const
     D inObjType                      7    const

     DoutLib           s             10    inz

      /Free
       RtvObjLib(inObj:inObjType:outLib);
       Return outLib;
      /End-free

     PGetObjLib        e

     PGetDirectory     b
     DGetDirectory     pi                  like(scDir)

     Dfolder           s             60    inz('*')
     Dindex            s              3  0

      /Free
       RtvFlrPrefix(folder);
       index = %scan('*':folder);
       %subst(folder:index) = *blank;
       //***  Begin Chg  ***  06/03/16  **********************************
       //Return folder;
       If folder <> *blanks;
        //folder = %trim(folder) + '/';
       Endif;
       CrtDftDir();
       Return %trim(folder) + dftDir;
       //***  End   Chg  ***  06/03/16  **********************************
      /End-free

     PGetDirectory     e
** Choices
Comma Separated Value (.csv)                      csv
Microsoft Excel (.xls)                            xl4
Text - Delimited (.txt)                           udt
Text - Plain (.txt)                               txt
Text - Tab delimited (.txt)                       tdt
Extensible Markup Language (.xml)                 xml

