     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -                          *
     H StoneRiver is a trademark of StoneRiver, Inc.')                                     *
     H option(*nodebugio:*srcstmt)                                                         *
     H dftactgrp(*no) actgrp(*Caller)                                                      *
     H/TITLE F8069R1                                                                       *
    H**************************************************************************************
    H*  Description:  Converts the License Panel Rate Code from holding file              *
    H*                to the License Panel Rate Code Table FTLPP.                         *
    H*                                                                                    *
    H*  Created: 04/30/2013 Programmer C:284   PROJECT:147MAS                             *
    H*                                                                                    *
    H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     H*                                                                                    *
     FFTLPL1    UF A E           K DISK                                                    *
     F*                                                                                    *
     F*License Panel Rate Code Table                                                       *
     F*                                                                                    *
     FFTLPPX    IF   E             DISK                                                    *
     F*                                                                                    *
     F* License Panel Rate Code Table Holding File                                         *
     F*                                                                                    *
     FFTLPXRL1  IF   E           K DISK                                                    *
     F*                                                                                    *
     F* Rate Code to Discount Code Mapping File                                            *
     FF8069O1   O    E             printer OFLIND(*IN85)
     F*
     F* List Rate Code not found in Rate code to Disc code mapping file
     D**************************************************************************************
     D* DATA STRUCTURE                                                                     *
     D**************************************************************************************
     DPSDS           ESDS                  Extname(SZQ1P)                                  *
     D*                                                                                    *
     D**************************************************************************************
     D* STAND ALONE VARIABLES                                                              *
     D**************************************************************************************
     D kklic#          s                   like(lplic#)                                    *
     D kkefdt          s                   like(lpefdt)                                    *
    C*                                                                                    *
     C     ftlpkey       klist                                                             *
     C                   kfld                    kklic#                                    *
     C                   kfld                    kkefdt                                    *
     C*                                                                                    *
     C**************************************************************************************
     C*  Read through flat file                                                            *
     C**************************************************************************************
     C*
      /Free
       write title;
       write head1;
       setll *start ftlppx;
       Read plx01;
       Dow not %eof(ftlppx);

          kkefdt = 1130101;
          kklic# = lxlic#;

          If lxlic# <> *blanks;
             Chain ftlpkey plp01;
             If %found(ftlpl1);                                 //Update record

                Chain lxratc pxr01;
                If %found(ftlpxrl1);                            //Update record

                   lpefdt = kkefdt;
                   lpfsn = lxfsn;
                   lplsn  = lxlsn;
                   lpname = lxname;
                   lplic# = lxlic#;
                   lpratc = lxratc;
                   lpcode = xrcode;
                   lpntwk = xrntwk;
                   lpstat = 'A';
                   lppgm = q1pgm;
                   lpuser = q1user;

                   lpdt = %dec(%date():*cymd);
                   lptime = %dec(%time():*hms);

                   update plp01;

                else;
                   if *In85 = *on;
                      write head1;
                   endif;

                   prratc = lxratc;
                   write dtl01;

                endif;

             else;                                                //Write record

                Chain lxratc pxr01;
                if %found(ftlpxrl1);

                   lpefdt = kkefdt;
                   lpfsn = lxfsn;
                   lplsn  = lxlsn;
                   lpname = lxname;
                   lplic# = lxlic#;
                   lpratc = lxratc;
                   lpcode = xrcode;
                   lpntwk = xrntwk;
                   lpstat = 'A';
                   lppgm = q1pgm;
                   lpuser = q1user;
                   lpipgm = q1pgm;
                   lpiusr = q1user;

                   lpdt = %dec(%date():*cymd);
                   lpidt = lpdt;
                   lptime = %dec(%time():*hms);
                   lpitim = lptime;

                   write plp01;

                else;

                   if *In85 = *on;
                      write head1;
                   endif;

                   prratc = lxratc;
                   write dtl01;


                endif;

             endif;
          endif;

          read plx01;

       enddo;
       *Inlr = *on;
      /End-Free
