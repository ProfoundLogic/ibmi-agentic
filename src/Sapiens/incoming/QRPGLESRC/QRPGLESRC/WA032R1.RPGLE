     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H/define CONVERT
      //****************************************************************
      //****************************************************************
     H/TITLE WA032R1 -  API - CROSS CHECK WMEMP LIVE/QUOTE FOR LOC fEIN
      //****************************************************************
      //  PROGRAM DESCRIPTION:                                         *
      //    This API will check that the FEIN passed in is not         *
      //    allocated to another employer.                             *
      //                                                               *
      //                                                               *
      //    CREATED:   07/11/14    C:180    SUPPORT: WA133 85852       *
      //                                    CLONED FROM WA032R         *
      //                                                               *
      //****************************************************************
      //  REVISIONS:                                                   *
      //                                                               *
      //****************************************************************
     FWMEML12   IF   E           K DISK
     D WA032R1         PR
     D   @@CO#                        3A
     D   @@FEIN                       9A
     D   @@EMP#                       9A
     D   @@ERR                        1A

     D WA032R1         PI
     D   @@CO#                        3A
     D   @@FEIN                       9A
     D   @@EMP#                       9A
     D   @@ERR                        1A

     D xxCo#           S              3  0
     D xxFein          S              9  0
     D xxEmp#          S              9  0

     D Digits          C                   '01234564789'

      /Free

       xxCo# = %int(@@CO#);

       If  @@Fein <> *Blanks;
           xxFein = %int(@@FEIN);
       Endif;

       xxEmp# = %int(@@EMP#);

       @@ERR  = *off;

       SETLL (xxCo#:xxFein) PEM01;
       If %equal(WMEML12);
          Reade (xxCo#:xxFein) PEM01;
          Dou %eof(WMEML12);
              If Ememp# <> xxEmp#;
                 @@ERR = *on;
                 Leave;
              Endif;
          Reade (xxCo#:xxFein) PEM01;
          Enddo;
       Endif;
       eval *inlr = *on;
      /End-Free
