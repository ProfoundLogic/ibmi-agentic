     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE LODWC1991 - Load WSRTP
     H DFTNAME(LODWC1991)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 12/02/14                                         *
      *  Programmer: C:283                                            *
      *  Project   : WC1991                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fwsckl     if   e           k disk    Rename(pck01 : pck01l)
     fwsrtl     if a e           k disk    Rename(prt01 : prt01l)

     d psds          esds                  ExtName(szq1p)
     d xxRTKY          s             10A   Inz(*Blanks)
     d cltID           s              3A   Inz(*Zeros)
     d xxCO#           s              3P 0 Inz(*Zeros)
     d holdRTCMWC      s              1A   Inz(' ')
     d holdRTCMWP      s              1A   Inz(' ')
     d holdRTCMAG      s              1A   Inz(' ')

     d/copy sPrDate

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      /Free
       GetClientID(cltID);

       Setll *Loval wsckl;
       Read wsckl;
       Dow Not %Eof(wsckl);
          xxCO# = CKco#;
          If CKidsa = '0000000000' Or CKidsa = *Blanks;
             If CKidst <> *Zeros;
                xxRTKY = %Editc(CKidst : 'X');
             Endif;
          Else;
             xxRTKY = CKidsa;
          Endif;

          If xxRTKY = *Blanks;
             xxRTKY = CKtrns + CKbnk# + %Editc(CKckdg : 'X');
          Endif;

          holdRTCMWC = ' ';
          holdRTCMWP = ' ';
          holdRTCMAG = ' ';

          Chain (xxCO# : xxRTKY) wsrtl;
          If Not %Found(wsrtl);
             Select;
             When cltID = '025';
                holdRTCMWC = 'Y';
                holdRTCMWP = 'Y';
                holdRTCMAG = 'Y';

             When cltID = '045';
                holdRTCMWP = 'Y';

             When cltID = '071';
                holdRTCMAG = 'Y';

             When cltID = '114';
                holdRTCMWC = 'Y';
                holdRTCMWP = 'Y';
                holdRTCMAG = 'Y';
             Endsl;

             RTco# = xxCO#;
             RTkey = xxRTKY;
             RTcmwc = holdRTCMWC;
             RTcmwp = holdRTCMWP;
             RTcmag = holdRTCMAG;
             RTspad = 'Y';

             RTidt = DateTo7(SystemDate());
             RTitm = TimeTo6(SystemTime());
             RTiusr = q1User;
             RTipgm = q1Pgm;
             RTdt = DateTo7(SystemDate());
             RTtime = TimeTo6(SystemTime());
             RTuser = q1User;
             RTpgm = q1Pgm;

             Write prt01l;
          Endif;
          Read wsckl;
       Enddo;

       *InLr= *On;
      /End-Free
