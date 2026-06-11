     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define CONVERT
      **************************************************************************
      *  Description: This program will process the selected groups and add to *
      *               data restrictions file.                                  *
      *                                                                        *
      *  Called From:   W7944C (when called with program id 'S0009R')          *
      *  Calls To:      None                                                   *
      *                                                                        *
      *  Created: 01/08/14  Programmer: C:279  Project: 114WKC                 *
      **************************************************************************
      *  Revisions:                                                            *
      *                                                                        *
      *                                                                        *
      **************************************************************************

     fwxgsl     if   e           k disk    rename(pgs01:pgs00)
     fsmazl     if   e           k disk    rename(paz01:paz00)
     fsmazl1    if   e           k disk
     fsmazp     if a e           k disk    rename(paz01:paz0p)

     d psds          esds                  extname(szq1p)

      * work variables
     d ##appl          s              1
     d ##auth          s              1
     d ##co            s              3  0
     d ##co#           s              3
     d ##cnam          s             40
     d ##fnd           s              3  0
     d ##fnd#          s              3
     d ##fnam          s             40
     d ##rtcd          s              1
     d ##sub           s              5
     d ##wcf           s              3
     d ##user          s             10
     d @@sub           s              5  0
     d @@fnd           s              3  0
     d @@empRng        s              9  0 inz(0)
     d isProcess       s               n

     d indtaara        s             15    dtaara(s0009dta)

      * procedure
     d getCmpNm        pr                  extPgm('WA001R')
     d ##co                                like(##co)
     d ##fnd                               like(##fnd)
     d ##cnam                              like(##cnam)
     d ##fnam                              like(##fnam)

     d getMCSub        pr                  extPgm('SA700R')
     d ##rtcd                              like(##rtcd)
     d ##co#                               like(##co#)
     d ##wcf                               like(##wcf)
     d ##sub                               like(##sub)
     d ##fnd#                              like(##fnd#)

     d/copy sPrDate
      /free

       // program processing
       Exsr ProgramStart;

       // main processing
       If isProcess = *on;
          Exsr Main;
       Endif;

       ExSr  ProgramEnd;

       //===================================================================
       // LoadData - Load data into database file
       //===================================================================
       Begsr LoadData;

          AZco#  = ##co;
          AZusr  = ##user;
          AZappl = ##appl;
          AZfemp = 0;
          AZtemp = 0;
          AZauth = ##auth;
          AZstat = 'A';
          AZipgm = q1pgm;
          AZiusr = q1user;
          AZitim = Timeto6(SystemTime());
          AZidt  = Dateto7(SystemDate());

          Write paz0p;
       Endsr;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          in indtaara;
          isProcess = *off;

          If indtaara <> ' ';
             ##user = %subst(indtaara:1:10);
             ##appl = %subst(indtaara:11:1);
             ##auth = %subst(indtaara:12:1);

             // Get Company Number
             getCmpNm(##co:##fnd:##cnam:##fnam);

             ##co# = %editc(##co:'X');
             isProcess = *on;
          Endif;

       Endsr;
       //===================================================================
       // ProgramEnd - End Processing
       //===================================================================
       Begsr ProgramEnd;

       *inlr = *on;
       Return;

       Endsr;

       //===================================================================
       // Main - Main processing
       //===================================================================
       Begsr Main;

          // read selected group from file WXGSP
          Setll ##co pgs00;
          Reade ##co pgs00;
          Dow not %eof;
             // If group is included.
             If GSiefl = 'I';

                Select;
                When ##appl = 'M';
                   Exsr ProcessMC;
                When ##appl = 'W';
                   Exsr ProcessWC;
                Endsl;

             Endif;

             Reade ##co pgs00;
          Enddo;

       Endsr;

       //===================================================================
       // ProcessWC - Process for Application flag 'W'
       //===================================================================
       Begsr ProcessWC;

          Chain (##co:##user:GSfnd:@@emprng) paz00;
          If not %found;
             AZsub# = 0;
             AZfnd  = GSfnd;

             Exsr LoadData;
          Endif;

       Endsr;

       //===================================================================
       // ProcessMC - Process for Application flag 'M'
       //===================================================================
       Begsr ProcessMC;

          ##rtcd = ' ';
          ##sub = ' ';
          ##wcf = %editc(GSfnd:'X');
          ##fnd# = ' ';

          // get Sub#/FND
          getMCSub(##rtcd:##co#:##wcf:##sub:##fnd#);

          If ##rtcd = ' ';
             @@sub = %int(##sub);
             @@fnd = %int(##fnd#);

             Chain (##co:##user:@@sub:@@fnd:@@emprng) paz01;
             If not %found;
                AZsub# = @@sub;
                AZfnd  = @@fnd;

                Exsr LoadData;
             Endif;
          Endif;

       Endsr;

      /end-free
