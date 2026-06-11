     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CHKLDAYSR - Check lost days
     H*
      *
      *---------------------------------------------------------------------------*
      * Created    : 09/20/16                                                     *
      * Programmer : C:180                                                        *
      * Support    : 43415                                                        *
      *---------------------------------------------------------------------------*
      *                                                                           *
      * This pgm fixes the lost days for a claim number.                    t     *
      *                                                                           *
      * The pgm takes 7 parameters:                                               *
      *  Company: 3A                                                              *
      *  Fund   : 3A                                                              *
      *  Fund Yr: 3A                                                              *
      *  Case   : 7A                                                              *
      *  From Dt: 7A (Cyymmdd)
      *  To Dt  : 7A (Cyymmdd)
      *  Mode   : Value '0' Inquiry Mode - Report is printed.                     *
      *           Value '1' Update Mode - Report is printed and                   *
      *                     Claims Master is updated.                        Comp.*
      *                                                                           *
      *---------------------------------------------------------------------------*
      * Revisions:
      *
      *
     FWDPAL5    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FWMCML1    UF   E           K DISK
      *
      * Entry
      *
     D CHKLDAYSR       PR
     D inCo                           3
     D inFnd                          3
     D inFyr                          3
     D inCase                         7
     D inFrDt                         7
     D inToDt                         7
     D inUpd                          1

     D CHKLDAYSR       PI
     D inCo                           3
     D inFnd                          3
     D inFyr                          3
     D inCase                         7
     D inFrDt                         7
     D inToDt                         7
     D inUpd                          1

      *
      * Stand Alone
      *
     D xxCo            S              3P 0 INZ
     D xxFnd           S              3P 0 INZ
     D xxFyr           S              3P 0 INZ
     D xxCase          S              7P 0 INZ
     D xxLost          S              5P 0 INZ
     D LostDays        S              7P 0 INZ
     D Time6           S               T   INZ
     D xxFrDt          S              7P 0 INZ
     D xxToDt          S              7P 0 INZ
     D Today           S               D   DATFMT(*ISO)

      /Free

       xxCo   = %int(inCo);
       xxFnd  = %int(inFnd);
       xxFyr  = %int(inFyr);
       xxCase = %int(inCase);

       If inFrDt > *blank;
          xxFrDt = %int(inFrDt);
       Endif;

       If inToDt > *blank;
          xxToDt = %int(inToDt);
       Else;
          xxToDt = 9999999;
       Endif;

       *IN78    = inUpd;
       Today    = %date();
       Time6    = %time();

       Chain(n) (xxCo:xxFnd:xxFyr:xxCase) WMCML1;

       If %found(WMCML1);
          Setll (xxCo:xxFnd:xxFyr:xxCase) WDPAL5;
          Reade (xxCo:xxFnd:xxFyr:xxCase) WDPAL5;
          Dow not %eof(WDPAL5);
             If PAretp = 'C' and (PAtrdt >= xxFrDt and PAtrDt <= xxToDt);
                Exsr SRDAYS;
                Reade (xxCo:xxFnd:xxFyr:xxCase) WDPAL5;
             Endif;
             Reade (xxCo:xxFnd:xxFyr:xxCase) WDPAL5;
          Enddo;
       Endif;

       // Print Report
       Except HDG;

       // Update Lost Days
       If *IN78;
          Chain (xxCo:xxFnd:xxFyr:xxCase) WMCML1;
          If %Found(WMCML1);
              CMlost = LostDays;
              CMLSDT = %dec(%char(Today:*cymd0):7:0);
              Update PCM01;
          Endif;
       Endif;

       *Inlr = *on;

       //-------------------------------------------------------
       // Check days                                           *
       //-------------------------------------------------------
       Begsr SRDAYS;
        xxLost = *Zeros;

        // Add weeks
        If PAwks <> *Zero;
           If CMdays <> *Zero;
              xxLost = xxLost + (PAwks * CMdays);
           Else;
              xxLost = xxLost + (PAwks * 7);
           Endif;
        Endif;

        // Add Days
        xxLost = xxLost + PAdays;


        //Determine to add or subtract to Total days
        Select;
        When (PAtype = 'A' and PAamt > *Zero) or
             (PAtype = 'P' and PAamt > *Zero) or
             (PAtype = 'M' and PAamt > *Zero) or
             (PAtype = 'Z' and PAamt > *Zero);
              LostDays = LostDays + xxLost;

        When (PAtype = 'R' and PAamt < *Zero) or
             (PAtype = 'S' and PAamt < *Zero) or
             (PAtype = 'V' and PAamt < *Zero) or
              LostDays = LostDays + xxLost;
        Other;
              LostDays = LostDays - xxLost;
        Endsl;
       Endsr;

      /End-Free
     OQSYSPRT   E            HDG              02
     O                                           14 'PGM:CHKLDDAYSR'
     O                                           75 'FIX LOST DAYS'
     O               78                          84 '- UPDATE'
     O              N78                          85 '- INQUIRY'
     O                                          124 'PAGE:'
     O                       PAGE          Z  +0001
     O          E            HDG         1  1
     O                       UDATE         Y
     O                       Time6             +001
     O                                           65 'FOR CLAIM'
     O                       xxCo             +0001
     O                                        +0000 '-'
     O                       xxFnd            +0000
     O                                        +0000 '-'
     O                       xxFyr            +0000
     O                                        +0000 '-'
     O                       xxCase           +0000
     O          E            HDG         1  1
     O                                           75 'Total Old Lost Days:'
     O                       CMLOST        J  +0001
     O                                           75 'Total New Lost Days:'
     O                       LostDays      J  +0001
     O          E            HDG         1  1
     O                                           50 '****** End of Report '
     O                                        +0000 '******'
