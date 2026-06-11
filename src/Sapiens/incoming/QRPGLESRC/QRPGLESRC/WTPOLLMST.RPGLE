     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLLMST  - Policy Location Master
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 05/25/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
      *  01/22/13  C:277  PROJECT: 133WNZ                             *
      *                   Compile Only                                *
      *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *                                                               *
    *  Program Modes: (@@Mode)                                      *
    *                 C - Change                                    *
    *                 D - Display                                   *
    *                 M - Mid-Term endorsement                      *
    *               In mode M - Tab parm will hold Endorsement #    *
    *===============================================================*

     d xxRrn           s              9    inz
     d xxMode          s              1    inz
     d xxNext          s             10    inz
     d xxTabInd        s              5    inz
     d xxQte           s              1    inz
      *

     d*Entry
     d LocEntry        pr                  extpgm('WTPOLLMST')
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d  @@TabInd                      5    Options(*nopass)
     d  @@Qte                         1    Options(*nopass)
     d LocEntry        pi
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d  @@TabInd                      5    Options(*nopass)
     d  @@Qte                         1    Options(*nopass)

     d/copy wPrLocMain

      /free
       // Relative Record # for employer detail.
       If %Parms > 0;
         xxRrn   = @@Rrn;
       Else;
         xxRrn   = *Zeros;
       Endif;

       // Program Mode.
       If %Parms > 1;
         xxMode = @@Mode;
       Else;
         xxMode = 'C';
       EndIf;

       // Next program
       If %Parms > 2;
         xxNext = @@Next;
       Else;
         xxNext = *blanks;
       EndIf;

       If %Parms > 3;
         xxTabInd = @@TabInd;
       Else;
         xxTabInd = *Zeros;
       Endif;
       If %Parms > 4;
         xxQte = @@Qte;
       Else;
         xxQte = ' ';
       Endif;
       LocMaster(xxRrn:xxMode:xxNext:xxTabInd:xxQte);
       @@RRN = xxRRN;
       @@Mode = xxMode;
       @@Next = xxNext;
       @@TabInd = *Zeros;
       //  @@Qte = *Blanks;
       *InLr = *On;
      /end-free
