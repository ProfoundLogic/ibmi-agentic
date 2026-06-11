     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE FIX42671 - Update Message File Removing PDS
     H*****************************************************************
     H* Description: Update the Message File Removing PDS
     H*
     H* 08/02/16 C:79 SUPPORT: 42671/099
     H*
     H*****************************************************************
     H** Revisions:
     H*****************************************************************
     H*
     H*****************************************************************
     H*
     F*
     FFtmip     UF   E             DISK
     F*
     FQSYSPRT   O    F  132        PRINTER
     F*
     D*
     D*================================================================
     D*  Data Structures
     D*================================================================
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*================================================================
     D*  Standalone fields
     D*================================================================
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D*================================================================
     D*  Copy Source
     D*================================================================
     D/copy sprdate
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*================================================================
     C* MAIN - Main program processing
     C*================================================================
      /free

         Read pMI01;

         dow  not %eof;

           if %subst(MImsg#:3:5) = '99997';
             select;
               When MImseq = 0;
                 MIdesc = 'The billed code did not match any adjudication'+
                           ' rules. Re-enter the';
               When MImseq = 1;
                 MIdesc = 'correct code or contact your CompSuite'+
                           ' software vendor.';
             endsl;

             Except Detail;

               sys_date = %date();
               sys_time = %time();
               MIdt = Dateto7(sys_date);
               MItime = TimeTo6(SystemTime());
               MIuser = Q1user;
               MIpgm = Q1pgm;
               update    pMI01;

           endif;

           Read pMI01;

         enddo;

         *inlr = *on;

      /end-free
     OQSYSPRT   e            Detail         1
     O                       MImsg#              10
     O                       MIdesc              90
