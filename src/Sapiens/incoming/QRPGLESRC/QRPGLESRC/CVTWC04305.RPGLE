     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')

     h/title CVTWC04305 - Conversion pgm to copy 1 WXFMP1 record into many
     h dftname(CVTWC04305)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h bnddir('STBNDDIR')
      //*********************************************************************
      //==============================================================*
      //  Created   : 12/27/24                                         *
      //  Programmer: C:300                                            *
      //  Project   : WC2994-Redesign of Addition Entry pgm for WC040305
      //  Description: This program will copy 1 WXFMP1 record into many*
      //               records and write into WXFMP1 file              *
      //===============================================================*
      //  Revisions:                                                   *
      *                                                                *
      *                                                                *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

      * Additional Information File
     fwxfmp1    o    e             disk    rename(pfm01:pfm0p)
     fwxfml11   uf   e           k disk    rename(pfm01:pfm00)
     f                                     prefix(L1_)

     d psds          esds                  extname(szq1p)

     d xxSeq#          s              3  0 inz

     d/copy sPrDate


      /free

       // We need to Read the data in WXFMP1 file.
       // If any of the F3C401 thru F3C413 fields are not blank then
       // For each field that is not blank create a new WXFMP1 record placing the
       // field's value into F3500A1 field
       // Increment F3seq# for each record starting with a value of 1
       // Remove the original WXFMP1 record once the data has been copied into new records


       // Read Additional Information File in loop
       Setll *loval pfm00;
       Read pfm00;
       Dow not %EOF(wxfml11);
          If L1_F3form = 'WC040305' and L1_F3seq# = 0;
             xxSeq# = 0;
             Exsr UpdFmp1;

             If L1_F3c401 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c401;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c402 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c402;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c403 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c403;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c404 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c404;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c405 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c405;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c406 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c406;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c407 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c407;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c408 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c408;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c409 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c409;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c410 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c410;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c411 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c411;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c412 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c412;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             If L1_F3c413 <> *Blanks;
                xxSeq# = xxSeq# + 1;
                F3seq# = xxSeq#;                      // Sequence #
                %subst(F3500a1:1:60) = L1_F3c413;     // Name in Generic 500 Chr 1
                Write pfm0p;
             Endif;

             Delete pfm00;                            // Delete old record
          Endif;
          Read pfm00;                                 // Read next record
       Enddo;

       *InLr = *On;


       Begsr UpdFmp1;                       // Update new record in WXFMP1
        Clear pfm0p;

        F3co#  = L1_F3co#;
        F3fnd  = L1_F3fnd;
        F3fyr  = L1_F3fyr;
        F3emp# = L1_F3emp#;
        F3div  = L1_F3div;
        F3form = L1_F3form;

        F3stat = 'A';                   // Status
        F3user = q1user;                // User
        F3pgm  = q1pgm;                 // Program
        F3time = Timeto6(SystemTime()); // Time
        F3dt   = Dateto7(SystemDate()); // Date Mintained
        F3iusr = q1user;                // Initial User
        F3ipgm = q1pgm;                 // Initial Program
        F3idt  = Dateto7(SystemDate()); // Initial Date
       Endsr;

      /end-free
