      //========================================================================*
     H/TITLE POPPBLITE - Populate Provder Bill Lite flag.
      //copyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
      // StoneRiver is a trademark of StoneRiver, Inc.')
      //*********************************************************************
      //*********************************************************************
      //
      //  Description:  This program populate the Provider Bill Lite flag to "Y"
      //  for certain clients in the Fee Schedule Base Control File (SFBPF)
      //
      //  Called from: SYPOSTPGM2 - Postload conversion program - Processing Program
      //  Calls to:    None
      //
      //
      //  Created on 10/22/2014 by C:274 for Project SY359
      //*********************************************************************
      // Revisions:
      //
      //
      //
      //

     FSSYLF     uf   e           k disk

     //=========================================================================
     //  Definition Specs
     //=========================================================================

     D xxCLT           s              3
     D XSKEY           s              2

     D                 DS
     D  WKFBST02               1      2
     D  WKPOS1                 1      1
     D  WKPOS2                 2      2

     D RtvCltid        PR                  EXTPGM('RTVCLTID')
     D xxCLT                          3

     //=========================================================================
     //  KLIST definitions
     //=========================================================================
    
     // Fee Schedule Base Control File
     C     FBky          klist
     C                   KFld                    XSKEY


     //=========================================================================
      // Main Line processing
     //=========================================================================

      // Get Client
      /Free
       RtvCltid(xxCLT);
      /End-Free

     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
      /FREE
       CHAIN FBKY SFB01;
       if %found(SSYLF);
         if xxCLT = '133'
               or xxCLT = '136'
               or xxCLT = '139';
           WKFBST02 = FBST02;
           WKPOS1 = 'Y';
           FBST02 = WKFBST02;
           UPDATE SFB01;
         endif;
       endif;

       *inlr = *on;

       //**  End of Program
      /END-FREE
