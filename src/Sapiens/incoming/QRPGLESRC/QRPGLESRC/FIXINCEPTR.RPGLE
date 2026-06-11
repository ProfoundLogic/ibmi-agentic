     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FIXINCEPTR - Fix inception date
     H DFTNAME(FIXINCEPTR)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     h**********************************************************************
    *===============================================================*
    *  Created   : 11/18/13                                         *
    *  Programmer: C:180                                            *
    *  Support   : WA090 82964                                      *
    *===============================================================*
    *  Revisions:                                                   *
      *  12/18/13   C:180 Support WA090 82964                         *
      *                   Remove parms for postload execution         *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments from the top of Revisions.         *
    * ------------------------------------------------------------- *
    *===============================================================*

     FWMEML     UP   E           K DISK
     FWDELL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     **** Begin Delete *** 12/18/13 **********************************
     D** FixInceptR      PR
     D** @UPD                         1A   Const
     D** @STAT                        1A   Const

     D** FixInceptR      PI
     D** @UPD                         1A   Const
     D** @STAT                        1A   Const
     **** End   Delete *** 12/18/13 **********************************
     **** Begin Add    *** 12/18/13 **********************************
     D @UPD            C                   CONST('1')
     D @STAT           C                   CONST('1')
     **** End   Add    *** 12/18/13 **********************************

     DDETKY          E DS                  extname(WMEML:*key)

     D POLNUM          DS
     D   ELCO#
     D     Dash1                      1    INZ('-')
     D   ELFND
     D     Dash2                      1    INZ('-')
     D   ELEMP#
     D     Dash3                      1    INZ('-')
     D   ELDIV
     D     Dash4                      1    INZ('-')
     D   ELFYR

     D InceptDate      S                   LIKE(EMEFD1) INZ
     D OldIncDate      S                   LIKE(EMEFD1) INZ
     D Count1          S              5  0 INZ
     D Count2          S              5  0 INZ
     D Count3          S              5  0 INZ
     D Error1          C                   'Policy year detail not found.'
     D Time6           S               T   INZ

      /Free
        OldIncDate   = emefd1;
        InceptDate   = *Zeros;
        exsr GetIncept;

        If *IN60;
             If InceptDate <> OldIncDate;
                If *IN77;
                   EMEFD1 = Inceptdate;
                   update PEM01;
                Endif;
                exsr print;
                Count1 = Count1 + 1;
             Else;
                Count2 = Count2 + 1;
             Endif;
        Else;
            exsr print;
            Count3 = Count3 + 1;
        Endif;
      /End-free

     CLR OF              EXCEPT    HDG
      /Free
       //*----------------------------------------------------------------
         BEGSR *inzsr;
       //*----------------------------------------------------------------
          *IN77 = @UPD;
          *IN78 = @STAT;
          Time6 = %Time();
          Except Hdg;
         ENDSR;

       //*----------------------------------------------------------------
         BEGSR GetIncept;
       //*----------------------------------------------------------------
           *IN60 = *On;
           setll %kds(DETKY) PEL01;
           If %equal;
              reade %KDS(DETKY) PEL01;
              If not %eof;
                 InceptDate = elpfdt;
              Else;
                 *IN60 = *Off;
              Endif;
           Else;
              *IN60 = *Off;
           Endif;
         ENDSR;

       //*----------------------------------------------------------------
         BEGSR Print;
       //*----------------------------------------------------------------
          If *INOF;
             Except Hdg;
          Endif;

          If *IN60;
             Except Det;
          Else;
             Except Err;
          Endif;

         ENDSR;
      /End-free
     OQSYSPRT   E            HDG            1 02
     O                                           14 'PGM:FIXINCEPTR'
     O                                           75 'FIX INCEPTION DATE'
     O               77                          84 '- UPDATE'
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         1
     O                       UDATE         Y
     O                       Time6             +001
     O          E            HDG         1
     O                                           34 'TERM'
     O                                           54 'INCEPTION'
     O                                           68 'INCEPTION'
     O          E            HDG         1
     O                                           13 'POLICY NUMBER'
     O                                           38 'EFF DATE'
     O                                           55 'DATE - NEW'
     O                                           69 'DATE - OLD'

     O          E            DET         1
     O                       POLNUM
     O                       ELPFDT              40 ' -  -  -  '
     O                       InceptDate          55 ' -  -  -  '
     O                       OldIncDate          69 ' -  -  -  '

     O          E   N78      ERR         1
     O                       EMCO#
     O                                              '-'
     O                       EMFND
     O                                              '-'
     O                       EMEMP#
     O                                              '-'
     O                       EMDIV
     O                       Error1            +005
     O                       OldIncDate          69 ' -  -  -  '

     O          T    LR                 02
     O               77                          14 'Fixed.........'
     O              n77                          14 'To be fixed...'
     O                       Count1        1   +001
     O              n78                          35 'OK.......'
     O              n78      Count2        1   +001
     O              n78                          60 'No Detail....'
     O              n78      Count3        1   +001
