     hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR')
     h/define   PROFOUNDUI
     h**********************************************************************
     h/title WTGRPCPY - Group Copy
     h**********************************************************************
     h*  Description:  This program will prompt for group copy information
     h*
     h*  Created: 04/05/18   C:33    Project: 150WCF
     h**********************************************************************
     h*  Revisions:
     h*
     h**********************************************************************
     fwtgrpcpy  cf   e             workstn extfile('WTGRPCPY')
     f                                     handler('PROFOUNDUI(HANDLER)')

     fsmcol     if   e           k disk    rename(pco01:pco00)
     fsmupl     if   e           k disk    rename(pup01:pup00)

     d clientID        s              3    inz
     d intCo#          s              3  0 inz
     d isUsedWCMC      s              1    inz
     d isUsedWCMS      s              1    inz
     d isUseWCMC       s              1n
     d psds          esds                  extname(szq1p)

     d*entry
     dWtGrpCopy        pr                  extpgm('WTGRPCPY')
     d inGroup                        3  0
     d outRtCd                       10    options(*nopass)
     dWtGrpCopy        pi
     d inGroup                        3  0
     d outRtCd                       10    options(*nopass)

     d ClearErrors     pr
     d ErrorCheck      pr              n
     d GetClientID     pr                  extpgm('RTVCLTID')
     d clientID                       3
     d Main            pr
     d ProgramStart    pr

     dCopyWCMC         pr                  extpgm('F0060C03')
     d outMode                       10    const
     d outGrpTo                       3    const
     d outGrpFrom                     3    const
     d inUsed                         1

     dCopyWCMS         pr                  extpgm('W0010C03')
     d outMode                       10    const
     d outGrpTo                       3    const
     d outGrpFrom                     3    const
     d inUsed                         1
     d outNxtEmp#                     9    const
     d outNxtQte#                     9    const
     d outNxtCase#                    9    const
     d outYearInfo                    1    const

     dGroupIsUsed      pr             1n

     d/copy sPrCompany
     d/copy sPrGenPop
     d/copy sPrModule
     d/copy wPrUsrPrms
     d/copy wPrGroup

      /free

       // Main Procesing
       Main();

       *inlr = *on;

      /end-free
       //===================================================================
       // ClearErrors - Clears Error Indicators
       //===================================================================
     p ClearErrors     b
     d ClearErrors     pi

      /free

       Clear erGrpTo;
       Clear erNxtCase#;
       Clear erNxtEmp#;
       Clear erNxtQte#;

      /end-free

     p ClearErrors     e
       //===================================================================
       // ErrorCheck - Error Check
       //===================================================================
     p ErrorCheck      b
     d ErrorCheck      pi              n

     D isError         s               n   Inz(*Off)

      /free

       If scGrpTo <= *zero;
          msGrpTo = 'WCR0502';
          erGrpTo = *on;
          isError = *on;
       EndIf;

       Select;
       When ValidGrp(scGrpTo) = *on;
          msGrpTo = 'WC30001';
          erGrpTo = *on;
          isError = *on;
       When GroupIsUsed = *on;
          msGrpTo = 'WC40045';
          erGrpTo = *on;
          isError = *on;
       Endsl;

       If scNxtCase# <= *zero;
          msNxtCase# = 'WCR0502';
          erNxtCase# = *on;
          isError    = *on;
       EndIf;

       If scNxtEmp# <= *zero;
          msNxtEmp# = 'WCR0502';
          erNxtEmp# = *on;
          isError   = *on;
       EndIf;

       If scNxtQte# <= *zero;
          msNxtQte# = 'WCR0502';
          erNxtQte# = *on;
          isError   = *on;
       EndIf;

       Return isError;

      /end-free

     p ErrorCheck      e

       //===================================================================
       // Check to see if the group is already used in any file
       //===================================================================
     pGroupIsUsed      b
     dGroupIsUsed      pi             1n

      /Free
       CopyWCMS('VERIFY':%editc(scGrpTo:'X'):%editc(scGrpFrom:'X'):
        isUsedWCMS:*blanks:*blanks:*blanks:scIncYear);
       If isUseWCMC;
          CopyWCMC('VERIFY':%editc(scGrpTo:'X'):%editc(scGrpFrom:'X'):
           isUsedWCMC);
       Endif;
       Return (isUsedWCMS = '1' or isUsedWCMC = '1');
      /End-free
     pGroupIsUsed      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /free

       ProgramStart();

       // Process until cancel button is pressed or no errors
       Dou btnCancel = *on or ErrorCheck = *off;
          ExFmt rcdGrpCopy;
          ClearErrors();
       Enddo;

       Select;
       When btnCancel = *on;
          *inlr = *on;
          Return;
       When btnCopy = *on;
          PopOneBtn('Group Copy will be submitted to batch.':
           'Notification');
          CopyWCMS('COPY':%editc(scGrpTo:'X'):%editc(scGrpFrom:'X'):
           isUsedWCMS:%editc(scNxtEmp#:'X'):%editc(scNxtQte#:'X'):
           %editc(scNxtCase#:'X'):scIncYear);
          If isUseWCMC;
             CopyWCMC('COPY':%editc(scGrpTo:'X'):%editc(scGrpFrom:'X'):
              isUsedWCMC);
          Endif;
       Endsl;

      /end-free
     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

     d suffix          s              3    Inz(' ')

      /free

       If not IsModEnabled('Group Copy');
          PopOneBtn('Group Copy has not been enable on your system.':
           'Invalid Option');
          Return;
       Endif;

       isUseWCMC = IsModEnabled('Managed Care');

       GetClientID(clientID);

       intCo# = UserCo#(q1User);

       scGrpFrom  = inGroup;
       scIncYear  = '1';
       scNxtCase# = 1;
       scNxtEmp#  = 1;
       scNxtQte#  = 50000000;

      /end-free
     p ProgramStart    e
