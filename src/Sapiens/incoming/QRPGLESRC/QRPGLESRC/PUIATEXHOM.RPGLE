     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE PUIATEXHOM - Atrium exit program to get homepage code
     h option(*nodebugio: *srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:  Atrium exit program to get homepage code*
     H*                                                               *
     H* CALLED BY: Atrium                                             *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* CREATED by: C:191        PROJECT: Transform                   *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************

     fsmupl     if   e           k disk    rename(pup01:pup00)
     fsmugl     if   e           k disk    rename(pug01:pug00)

     D PUIATEXHOM      PR                  ExtPgm('PUIATEXHOM')
     D  user                         10
     D  home                          9  0
      *
     D PUIATEXHOM      PI
     D  user                         10
     D  home                          9  0

     D/copy sPrPrprty

     D userGroup       s             10    inz
     D company         s              3  0 inz

      /free
        home = 0;
        userGroup = *blanks;

        //Get userdefault;
        Chain(n) user pup00;
        If %Found(smupl);
          home = UPhome;
          company = UPco#;
          userGroup = UPugrp;
        Endif;

        //Get User Group default if user default not set
        If home = 0 And userGroup <> *Blanks;
          Chain(n) (company:userGroup) pug00;
          If %Found(smugl);
            home = UGhome;
          Endif;
        Endif;

        //Get System default if no home page found yet
        If home = 0;
          Monitor;
            home = %Int(%Trim(SysProperty('AtDefaultHomePage')));
          On-error;
            home = 0;
          EndMon;
        Endif;
        *inlr = *On;
        Return;
      /end-free
