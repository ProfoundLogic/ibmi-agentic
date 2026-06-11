     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title FIX35309- Correct User Ids That where set to QTMHHTTP
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h**********************************************************************
     h*===============================================================*
     h*  Created   : 06/15/15                                         *
     h*  Programmer: C:280                                            *
     h*  Support   : Phaseware 35309 TASB                             *
     h*===============================================================*
     h*  Revisions:                                                   *
     h*                                                               *
     h*                                                               *
     h* ------------------------------------------------------------- *
     h*  NOTE: Please add comments to the top of Revisions.           *
     h* ------------------------------------------------------------- *
     h*===============================================================*
     f*
     ffaeop     uf   e             disk
     f*
     ffaehl     if   e           k disk
     f*
     ffaeul     if   e           k disk
     f*
     ffaedl     if   e           k disk
     f*
     ffaepl     if   e           k disk
     f*
     ffaesl1    if   e           k disk
    F*
     FQSYSPRT   O    F  132        printer oflind(*inof)
    F*
     D*
     Ddate             s              6s 0
     D*
     f*
     d* Stand alone variables.
     dpsds           esds                  extname(szQ1p)
    C*
    C* Define Fields
    C*
     C     *like         define    eouser        beouser
     C     *like         define    eoiusr        beoiusr
    C*
    C* Paramater for Update
    C*
     C     *entry        plist
     C                   parm                    Updt              1
     C*
     C* Main program processing
     C*

      /Free

        date  = %dec(%date():*mdy);

        except header;

        Read peo01;

         Dow not %eof(faeop);

          If eouser = 'QTMHHTTP';

           If EOblst = 'D' and EOiusr = 'QTMHHTTP';

                chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faesl1;

                 if %found(faesl1);
                   Beoiusr = eoiusr;
                   eoiusr = esiusr;
                 endif;

              endif;

           Select;

             When EObtyp = 'N';

              chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faehl;

               if %found(faehl);

                 Beouser = eouser;
                 eouser = ehuser;

                 if updt = 'Y';
                 update peo01;
                 endif;

               endif;

             When EObtyp = 'H';

              chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faeul;

               if %found(faeul);
                 Beouser = eouser;
                 eouser = euuser;

                 if updt = 'Y';
                 update peo01;
                 endif;

               endif;

             When EObtyp = 'D';

              chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faedl;

               if %found(faedl);
                 Beouser = eouser;
                 eouser = eduser;

                 if updt = 'Y';
                 update peo01;
                 endif;

               endif;

             When EObtyp = 'X';

              chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faepl;

               if %found(faepl);

                 Beouser = eouser;
                 eouser = epuser;

                 if updt = 'Y';
                 update peo01;
                 endif;

               endif;

             When EObtyp = 'V';

              chain (eoco#:eosub#:eofnd:eofyr:eocase:eoeob#) faesl1;

               if %found(faesl1);

                 Beoiusr = eoiusr;
                 eoiusr = esiusr;
                 Beouser = eouser;
                 eouser = esuser;

                 if updt = 'Y';
                 update peo01;
                 endif;

               endif;

           Endsl;

         if *inof = *on;
           except header;
           *inof = *off;
         Endif;

         except detail;

         Endif;

         Read peo01;

        Enddo;

        *inlr = *on;

      /End-Free
     O*
     Oqsysprt   e            header      3  2
     O                       Q1PGM               10
     O                                           47 'Fix User Id'
     O                       Date                70 '  /  /  '
     O*
     O          e            header         1
     O                                           29 'Before'
     O                                           40 'After'
     O*
     O          e            header         1
     O                                           30 'Initial'
     O                                           42 'Initial'
     O                                           54 'Before'
     O                                           65 'After'
     O*
     O          e            header         2
     O                                            5 'Fund'
     O                                           11 'Year'
     O                                           19 'Case #'
     O                                           29 'User'
     O                                           41 'User'
     O                                           53 'User'
     O                                           65 'User'
     O*
     O          e            detail         1
     O                       eofnd         z      5
     O                       eofyr         z     11
     O                       eocase        z     20
     O                       beoiusr             32
     O                       eoiusr              44
     O                       beouser             56
     O                       eouser              68
