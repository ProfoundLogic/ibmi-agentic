     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h/title CVTCXNOPR - Conversion program CX Reasons for No Prorating
     h dftname(CVTCXNOPR)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
     H**************************************************************************************
     H*  Description:  Conversion program CX Reasons for No Prorating                      *
     H*                                                                                    *
     H*  Created: 09/18/25   Programmer C:342   Project:WC3014                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     f*Cancellation Reason
     fwtcrp     uf   e             disk
     d psds          esds                  extname(szq1p)
     d/copy sPrDate
     d matched         s               n   inz

       read wtcrp;
       dow not %eof(wtcrp);

          // Reset the flag
          matched = *off;
          // Check if CRNCCI matches any of the target values
          select;
          when CRncci = '03';
             matched = *on;
          when CRncci = '08';
             matched = *on;
          when CRncci = '09';
             matched = *on;
          when CRncci = '10';
             matched = *on;
          endsl;

          if matched = *on;
             CRnopro = 'Y';
          else;
             CRnopro = ' ';
          endif;

          CRuser = q1user;
          CRpgm  = q1pgm;
          CRdt   = Dateto7(SystemDate()); // Date
          CRtime = Timeto6(SystemTime()); // Time
          // Update the record
          update pcr01;

          // Read next
          read wtcrp;
       enddo;
       *inlr = *on;
       return;
