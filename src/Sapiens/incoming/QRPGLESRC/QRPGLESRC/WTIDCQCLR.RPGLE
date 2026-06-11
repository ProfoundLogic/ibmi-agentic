     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE WTIDCQCLR  - Clear IDC Quarterly Log File
     H DFTNAME(WTIDCQCLR)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 10/29/19                                         *
      *  Programmer: C:180                                            *
      *  Project   : WC2604 - NCCI Indemnity Data Call                *
      *---------------------------------------------------------------*
      *  Description: Clear IDC Quarterly Log File
      *
      *
      *---------------------------------------------------------------*
      * Revisions:
      *
      * 06/06/25    C:344  Support 133/118331
      *                    Include maintenance fields when updating IDC file
      *
      * 01/23/25    C:053  Support 118108
      *                    Check to ensure a month end date
      *
      * 01/14/25    C:340  Project WC2995
      *                    Recompile Only;
      *
      *---------------------------------------------------------------*
     FWMIDCSC   IF   E             DISK    Infds(fdsIDCSC)
     FWMIDCQTL1 UF   E           K DISK    Infds(fdsIDCTR)
      *** Beg Add *** 01/23/25 **********************************
     FWMIDCQTL8 IF   E           K DISK    rename(PQT01:PQT08) Prefix(x)
      *** End Add *** 01/23/25 **********************************
      * *Entry
      *
     D WTIDCQCLR       PR
     D inRRN                          9A   Const
     D outCo#                         3A
     D outDate                        8A

     D WTIDCQCLR       PI
     D inRRN                          9A   Const
     D outCo#                         3A
     D outDate                        8A

       //**** Beg Add **** 06/06/25 ****************************************
     D psds          esds                  ExtName(szq1p)
       //**** End Add **** 06/06/25 ****************************************
     D fdsIDCSC        DS
     D  dbSCrrn              397    400B 0

     D fdsIDCTR        DS
     D  dbTRrrn              397    400B 0


     D intRRN          S              9  0 INZ

      /Free

       //Retrieve Submission Control Record

       intRRN = %int(inRRN);
       chain(n) intrrn WMIDCSC;
       If %found(WMIDCSC);

       //Clear Quarterly Log records

          Setll (CCbchId) PQT01;
          If %equal(WMIDCQTL1);
             reade CCbchId PQT01;
             If not %eof(WMIDCQTL1);
                outCo# = %Editc(QTco#:'X');
                outDate = %Subst(QTtrnDate:5:2) + %Subst(QTtrnDate:7:2) +
                          %Subst(QTtrnDate:1:4);
             Else;
      *** Beg Add *** 01/23/25 **********************************
       //If no date found try without status select file condition.

                Setll (CCbchId) PQT08;
                If %equal(WMIDCQTL8);
                   reade CCbchId PQT08;
                   Dow not %eof(WMIDCQTL8);

                      If %subst(xQTtrnDate:1:4) = '0331' or
                         %subst(xQTtrnDate:1:4) = '0630' or
                         %subst(xQTtrnDate:1:4) = '0930' or
                         %subst(xQTtrnDate:1:4) = '1231';

                         outCo# = %Editc(xQTco#:'X');
                         outDate = %Subst(xQTtrnDate:5:2) +
                                   %Subst(xQTtrnDate:7:2) +
                                   %Subst(xQTtrnDate:1:4);
                      Else;
                         reade CCbchId PQT08;
                      Endif;
                   Enddo;
                Else;
      *** End Add *** 01/23/25 **********************************
                     outCo# = *Blanks;
                     outDate = *Blanks;
      *** Beg Add *** 01/23/25 **********************************
                Endif;
      *** End Add *** 01/23/25 **********************************
             Endif;

             Dow not %eof(WMIDCQTL1);
                QTsts = 'Z';
                //**** Beg Add **** 06/06/25 *******************************
                QTuser  = Q1user;
                QTpgm   = Q1pgm;
                QTtime  = %int(%char(%time():*ISO0));
                QTdt    = %dec(%date():*cymd);
                //**** End Add **** 06/06/25 *******************************
                Update PQT01;
                reade CCbchId PQT01;
             Enddo;

          Endif;

       Endif;
       *inlr = *On;
