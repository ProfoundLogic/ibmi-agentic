     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title LODSTTSP - Add new records to Schema Type File
     h dftname(LODSTTSP)
     h option(*nodebugio: *srcstmt)
     h/define   PROFOUNDUI
     h bnddir('STBNDDIR')
     h**********************************************************************
      *====================================================================*
      *  Description:  This Program will add new records to the Schema     *
      *                Type File.                                          *
      *                                                                    *
      *  To Build a record - copy the 4 lines which load the work fields   *
      *  and call the subroutine - populating fields with new data.        *
      *                                                                    *
      *  Called from:  SYPOSTCV                                            *
      *                                                                    *
      *  Created on 11/16/17 by C:65 for Project: 102WHK                   *
      *====================================================================*
      *  Revisions:                                                        *
      *  01/29/19 C:180 Project 102WHL                                     *
      *                 SC state forms in Scriptura - Added 18 and 19      *
      *                                                                    *
      *  06/13/18 c:62 Project 102WHM
      *                -Changed program to use compile time arrays         *
      *                -Added IA-1 and 26B
      * ------------------------------------------------------------------ *
      *  NOTE: Please add comments to the top of Revisions.                *
      * ------------------------------------------------------------------ *
      *====================================================================*
     fsttsl     if a e           k disk    rename(pts01:pts00)

     d psds          esds                  extname(szq1p)

     d* Compile Time Arrays
     d cat             s              5    dim(#Records) CtData PerRcd(1)       Category
     d type            s            100    dim(#Records) CtData PerRcd(1)       Schema Type
     d pPgm            s             10    dim(#Records) CtData PerRcd(1)       Processing Program

     d* Work Varibales
     d i               s              5  0
     d #Records        c                   3

      /copy sPrDate

      /free

       For i=1 to #records;
          Exsr WriteRecord;
       EndFor;

       *inlr  = *on;

       //===================================================================
       // Write a table record
       //===================================================================
       BegSr WriteRecord;

          Chain (cat(i):type(i)) pts00;
          If not %found;
             Clear pts00;
             TSCat   = cat(i);
             TSType  = type(i);
             TSPPgm  = pPgm(i);
             TSpgm   = q1Pgm;
             TSuser  = q1User;
             TStime  = Timeto6(SystemTime());
             TSdt    = Dateto7(SystemDate());
             TSipgm  = q1Pgm;
             TSiusr  = q1User;
             TSitim  = Timeto6(SystemTime());
             TSidt   = Dateto7(SystemDate());
             Write pts00;
          Endif;

       Endsr;

      /end-free
** cat - Category
CS
CS
CS
** type - schema type
IA-1
28B
SC Form 18 and 19
** pPgm - processing program
WTFROIA1
WTADDMED
WTPERSTR
