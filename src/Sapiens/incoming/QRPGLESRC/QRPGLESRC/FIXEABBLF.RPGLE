     hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIXEABBLF- Fix BRS Inv proc flag in file FAEOP.
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 11/19/14                                         *
     h*  Programmer: C:296                                            *
     h*  Support   : 87194                                            *
     h*===============================================================*
     h*  Revisions:                                                   *
     h*                                                               *
     h*                                                               *
     h* ------------------------------------------------------------- *
     h*  NOTE: Please add comments to the top of Revisions.           *
     h* ------------------------------------------------------------- *
     h*===============================================================*
     f*
     ffaeap     uf a e             disk

     d* Stand alone variables.
     d*
     dpsds           esds                  extname(szQ1p)
     d*---------------------------------------------------
     d GetSysflg       pr                  extpgm('SA001R')
     d ##wcif                         1A
     d ##mcif                         1A
     d ##wcin                         1A
     d ##ar                           1A
     d ##ntwk                         1A
     d ##ur                           1A
     d ##med                          1A
     d ##edi                          1A
     d ##wcdk                         1A
     d*---------------------------------------------------
     d ##wcif          s              1A   inz(*blanks)
     d ##mcif          s              1A   inz(*blanks)
     d ##wcin          s              1A   inz(*blanks)
     d ##ar            s              1A   inz(*blanks)
     d ##ntwk          s              1A   inz(*blanks)
     d ##ur            s              1A   inz(*blanks)
     d ##med           s              1A   inz(*blanks)
     d ##edi           s              1A   inz(*blanks)
     d ##wcdk          s              1A   inz(*blanks)
     C*
     C* Main program processing
      /Free
       exsr SrMain;

       begsr SrMain;

         GetSysflg (##wcif: ##mcif: ##wcin: ##ar: ##ntwk: ##ur: ##med:
                    ##edi: ##wcdk);
           if ##ar = 'Y';
              exsr SrUpdFAeop;
           endif;

         *inlr = *on;
       endsr;

       begsr SrUpdFAeop;
         setll *start faeap;
         read pea01;

         dow not %eof(faeap);

           // if it is deleted EOMB then update BRS Inv proc flag
           // to P. So that this record is not picked up in invoice.
           if EAbblf <> 'P' and (EAepFl = 'D' or EAepFl = 'd');
              EAbblf = 'P';
              EAuser = Q1User;
              EApgm  = Q1pgm;
              EAtime = %dec(%Time():*hms);
              EAdt   = %dec(%Date():*cymd);
                update pea01;
           endif;
          read pea01;
         enddo;

       endSr;
      /End-Free

