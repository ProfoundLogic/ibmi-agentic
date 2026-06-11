     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FPIZIPLOC - Managed Care Zip Locality Module
     H NOMAIN
     H BNDDIR('FTBNDDIR')
     h**********************************************************************
     h**   PROGRAM DESCRIPTION:                                       *
     h**                                                              *
     h**    This program is used to perform various actions regarding *
     h**    zip codes and localities for states                       *
     h**                                                              *
      *===============================================================*
      *  Created   : 02/15/16                                         *
      *  Programmer: C:079                                            *
      *  Support   : 38898C                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     f* file definitions
     fFtzpl1    if   e           k disk

     d* prototype definitions
     d/copy FPRZIPLOC

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d* array definitions
     d*

     d* constant definitions
     d*

     d* standalone variables
     d*
     d kkZip           s                   like(ZPzip)
     d kkPls4          s                   like(ZPpls4)
     d kkSt            s                   like(ZPst)
     d kkEfDt          s                   like(ZPefDt)

     P* procedure implementations
     P*====================================================================
     P*  GetZip9Loc - Return locality for 9 digit zip code
     P*====================================================================
     P GetZip9Loc      B                   Export
     d GetZip9Loc      PI             2  0
     d inZip9                         9
     d inState                        2
     d inEfDt                         7  0
     d*
     d* standalone variables
     d*
     d outLoc          s              2  0 inz
      /free
       If %subst(inZip9:6:4) > '0000';
          Kkst = inState;
          Kkzip = %dec(%subst(inZip9:1:5):5:0);
          Kkpls4 = %dec(%subst(inZip9:6:4):4:0);
          Kkefdt = inEfDt;
          Setgt (kkSt:kkZip:kkPls4:kkEfDt) pzp01;
          Readp Pzp01;
          If %found(Ftzpl1) and (Zpst = Kkst) and (Zpzip = Kkzip)
           and (Zppls4 = Kkpls4);
             outLoc = ZPloc;
          Endif;
       Endif;

       Return outLoc;

      /end-free
     P GetZip9Loc      E
