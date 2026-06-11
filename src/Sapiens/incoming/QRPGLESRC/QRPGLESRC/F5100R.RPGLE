     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ******************************************************************
      ******************************************************************
      *   F4100R - Fee Schedule Adjustment Program                     *
      *                                                                *
      *   Created By : C:212 Project OK022                             *
      *                                                                *
      ******************************************************************
      *  REVISIONS:                                                    *
      *  05/02/17 C: 079 Project: GA038                                *
      *                  Add logic for GA                              *
      *                                                                *
      ******************************************************************
      *  INDICATORS USED:                                              *
      *                                                                *
      ******************************************************************
     FFAEOP     IF   E             DISK
     FFDCAL13   UF A E           K DISK
     FFDCALF    UF A E           K DISK
     F                                     RENAME(PCA01:PCA0F)
     FFDCAL19   IF   E           K DISK
     F                                     RENAME(PCA01:PCA19)
     FFDDDL2    UF A E           K DISK
     FFDDUL3    IF A E           K DISK
     FFDDHL3    IF A E           K DISK
     FFMPRL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK
     FFMP2LAL   IF   E           K DISK
     FFDDUL     IF   E           K DISK
     F                                     RENAME(PDU01:PDU02)
    F*
     Ffdmgp     o    e           k disk
    F*
     Ffdmgl6    uf   e           k disk
     F                                     rename(pmg01:pmg02)
    F*
     Ffdmgl5    if   e           k disk
     F                                     rename(pmg01:pmg03)
     Fftmil     if   e           k disk
     F*** Beg Add *** 05/02/17 **************************************
     Fststl     if   e           k disk
     F*** End Add *** 05/02/17 **************************************

     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11

      //  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
    D*
    D*  Error message breakdown into state and msg#.
    D*
     D                 ds
     D xxmsg                          7
     D  x1msg                         2    overlay(xxmsg)
     D  x2msg                         5    overlay(xxmsg:3)

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D MRAADJ          C                   CONST('MRA adjustment')

     D* standalone declaration

     D kkmiseq         s                   like(mimseq) inz
     D kkmgseq         s                   like(mgmseq) inz
     D kkmsg#          s                   like(mimsg#) inz
     D xxmseq          s                   like(mgmseq) inz
     D xxrcn           s                   like(carcn) inz
     D*** Beg Add *** 05/02/17 **************************************
     D kkAst           s                   like(STkey) inz
     D*** End Add *** 05/02/17 **************************************

      //  PLIST definitions

     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0          FAEOP

     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

      // KEY LISTS

     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN

     C     CAKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#

     C     CAKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKDSEQ

     C     CAKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE

     C     CAKY4         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKLIN#

     C     DDKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKPRCD

     C     DUKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    K1RVCD
     C                   KFLD                    KKRATE

     C     DHKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKPRCD

     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPRCD

     C     SVKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCMSV

      // Key list for Provider Auxiliary file

     C     P2KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV
     C                   KFLD                    KKFDAT

     C     P2KY1         KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKCMSV

     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
    C*
    C* Key List for Bill Entry Error Message File
     C     mgky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    carcn
     C                   kfld                    cadseq
    C*
     C     mgky1         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    carcn
     C                   kfld                    cadseq
     C                   kfld                    kkmgseq
    C*
     C     mgky2         klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C                   kfld                    xxmsg
     C                   kfld                    carcn
    C*
     C     miky          klist
     C                   kfld                    kkmsg#
     C                   kfld                    kkmiseq
    C*

     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    DURATE        KKRATE
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    DDPRCD        KKPRCD

     C     *LIKE         DEFINE    DURVCD        K1RVCD

     C     *LIKE         DEFINE    CACHRG        X3CHRG
     C     *LIKE         DEFINE    CAMRAA        X3MRAA
     C     *LIKE         DEFINE    CADIFF        X3DIFF

     C     *LIKE         DEFINE    CALIN#        X1LIN#

     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFEIN        KKFEIN
     C     *LIKE         DEFINE    CAMOD         KKMOD
     C     *LIKE         DEFINE    CAFDAT        KKFDAT

      // INITIALIZATION

     C                   Z-ADD     *ZEROS        HLDIFF           11 2
     C                   Z-ADD     *ZEROS        HLCHRG           11 2
     C                   Z-ADD     *ZEROS        HLPAID           11 2
     C                   Z-ADD     *ZEROS        HLTDIF           11 2
     C                   Z-ADD     *ZEROS        HLNEWP           11 2

     C                   Z-ADD     *ZEROS        X3CHRG
     C                   Z-ADD     *ZEROS        X3MRAA
     C                   Z-ADD     *ZEROS        X3DIFF

      // LOAD KEY FIELDS FOR EOB#

      /FREE
       CHAIN @@RRN PEO01;
       *IN99 = NOT %FOUND;

      /END-FREE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN

      // EXECUTE SUBROUTINE BASED ON BILL TYPE

      /FREE
       SELECT;

       WHEN EOBTYP = 'N';
         EXSR CRTMED;

       WHEN EOBTYP = 'D';
         EXSR CRTDEN;

       WHEN EOBTYP = 'H';
         EXSR CRTHOS;

       ENDSL;

       *inlr = *on;

       RETURN;

       //================================================================
       // CRTMED - Calculate FSMRA adjustment for Medical Bills
       //================================================================
       BEGSR CRTMED;

         //** The following logic is for Medical/Physician bills

         CHAIN CAKY1 PCA01;
         *IN99 = NOT %FOUND;

         DOW *IN99 = *OFF;

             // EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
             // OR HCFA ALLOWANCE.  PG 327 1.
             // EXCLUDE SURGERY CODES                                       ED

             IF EOAST = 'OK'
             AND CACPT4 >= 'A0000'
             AND CACPT4 <= 'C9999'
             OR EOAST = 'OK'
             AND CACPT4 >= 'E0001'
             AND CACPT4 <= 'Z9999'
             OR CACPT4 >= '10000'
             AND CACPT4 <= '69999';
               UPDATE PCA01;
               READE CAKY1 PCA01;
               *IN99 = %EOF;

             ELSE;

               x3mraa = CAmraa;
               x3chrg = CAchrg;
               x3diff = x3mraa - x3chrg;

               IF X3DIFF > 0;
                 CAdiff = x3diff;
                 HLdiff = HLdiff + CAdiff;
               ELSE;
                 CAdiff = 0;
               ENDIF;                                                         //CADIFF=0

      /END-FREE
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME

      /FREE
               UPDATE PCA01;
               READE CAKY1 PCA01;
               *IN99 = %EOF;

             ENDIF;

         ENDDO;                                                               //*IN99=*OFF

         HLtdif = HLchrg - HLpaid;

         IF HLDIFF <= HLTDIF;
           HLnewp = HLdiff;
         ELSE;
           HLnewp = HLtdif;
         ENDIF;                                                               //DIFF<TDIF

         IF HLNEWP > *ZERO;

           EXSR AdjustLines;

         ENDIF;                                                               //HLNEWP>0

       ENDSR;

       //================================================================
       // CRTDEN - Calculate FSMRA adjustment for Dental Bills
       //================================================================
       BEGSR CRTDEN;

         //** The following logic is for Dental bills

         CHAIN CAKY1 PCA01;
         *IN99 = NOT %FOUND;

         DOW *IN99 = *OFF;

             // EXCLUDE OKLAHOMA HCPCS CODES. HCPCS ARE PAID LESSER OF CHARGED
             // OR HCFA ALLOWANCE.  PG 327 1.

             IF EOAST = 'OK'
             AND CACPT4 >= 'A0000'
             AND CACPT4 <= 'C9999'
             OR EOAST = 'OK'
             AND CACPT4 >= 'E0001'
             AND CACPT4 <= 'Z9999';

               UPDATE PCA01;
               READE CAKY1 PCA01;
               *IN99 = %EOF;

             ELSE;

               x3mraa = CAmraa;
               x3chrg = CAchrg;
               x3diff = x3mraa - x3chrg;

               IF X3DIFF > 0;
                 CAdiff = x3diff;
                 HLdiff = HLdiff + CAdiff;
               ELSE;
                 CAdiff = 0;
               ENDIF;                                                         //CADIFF=0

      /END-FREE
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME

      /FREE
               UPDATE PCA01;
               READE CAKY1 PCA01;
               *IN99 = %EOF;

             ENDIF;

         ENDDO;                                                               //*IN99=*OFF

         HLtdif = HLchrg - HLpaid;

         IF HLDIFF <= HLTDIF;
           HLnewp = HLdiff;
         ELSE;
           HLnewp = HLtdif;
         ENDIF;

         IF HLNEWP > *ZERO;

           EXSR AdjustLines;

         ENDIF;

       ENDSR;

       //================================================================
       // CRTHOS - Calculate FSMRA adjustment for Hosptial Bills
       //================================================================
       BEGSR CRTHOS;
         //
         //** The following logic is for Hospital bills
         //
         CHAIN CAKY1 PCA01;
         *IN99 = NOT %FOUND;
         //
         DOW *IN99 = *OFF;

           // Alabama Hospitals flagged with an '*' are excluded from FSMRA

           IF EOAST = 'AL';

      /END-FREE
     C                   Z-ADD     CADSEQ        KKDSEQ
     C                   MOVE      'N'           XXSKIP            1

      /FREE
             CHAIN DUKY PDU02;
             *IN99 = NOT %FOUND;

             IF *IN99 = *OFF;

      /END-FREE
     C                   MOVE      DURVCD        KKRVCD
     C                   MOVE      DURATE        KKHCPC
     C                   MOVE      *ZEROS        KKUNTS
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   Z-ADD     CAFEIN        KKFEIN
     C                   Z-ADD     CAMOD         KKMOD
      //
      /FREE
             ENDIF;

             SETGT P2KY PP201;
             READPE P2KY1 PP201;
             *IN99 = %EOF;

             IF *IN99 = *OFF
             AND P2FL02 = '*';

      /END-FREE
     C                   MOVE      'Y'           XXSKIP
      //
      /FREE
             ENDIF;                                                           //*IN99 = *OFF

           ENDIF;                                                             //EOAST = AL

           // Exclude Surgery Codes
           //*** Beg Chg *** 05/02/17 ***********************************
           //*** IF CACPT4 >= '10000' AND CACPT4 <= '69999';
           If DUrate >= '10000' and DUrate <= '69999'
             and (EOast = 'OK' or EOast = 'AL');
           //*** End Chg *** 05/02/17 ***********************************
              XXSKIP = 'Y';
           ENDIF;                                                             //EOAST = AL
           //*** Beg Add *** 05/02/17 ***********************************
           if EOast = 'GA' and DUrvcd = 278;
              eval xxSkip = 'Y';
           endif;
           //*** End Add *** 05/02/17 ***********************************

             IF XXSKIP <> 'Y';

               x3mraa = CAmraa;
               x3chrg = CAchrg;
               x3diff = x3mraa - x3chrg;

               IF EOAST = 'AL';
                  HLDIFF = HLDIFF + CADIFF;
               ELSE;
                 IF X3DIFF > 0;
                   CAdiff = x3diff;
                   HLdiff = HLdiff + CAdiff;
                 ELSE;
                   CAdiff = 0;
                 ENDIF;
               ENDIF;                                                         //EOAST = AL

      /END-FREE
     C                   ADD       CACHRG        HLCHRG
     C                   ADD       CAPAID        HLPAID
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME

      /FREE
             ENDIF;                                                           //XXSKIP <> Y

             UPDATE PCA01;
             READE CAKY1 PCA01;
             *IN99 = %EOF;

         ENDDO;                                                               //*IN99=*OFF

         HLtdif = HLchrg - HLpaid;

         IF EOAST = 'AL';
           HLNEWP = HLDIFF;
         ELSE;
           IF HLDIFF <= HLTDIF;
             HLnewp = HLdiff;
           ELSE;
             HLnewp = HLtdif;
           ENDIF;
         ENDIF;                                                               //EOAST = AL

         IF HLNEWP > *ZERO;

           EXSR AdjustLines;

         ENDIF;

       ENDSR;

       //================================================================
       // AdjustLines - Adjust the values on line item and update
       //================================================================

       Begsr AdjustLines;

        Setll caky1 pca01;
        ReadE caky1 pca01;
        Dow not %eof(fdcal13);

          //*** Beg Chg *** 05/02/17 ********************************
          //***If CAdiff > 0;
          //***  CApaid = CApaid + CAdiff;
          //***  CAmra = CAmra - CAdiff;
        If hlNewp > 0 and CAdiff > 0;
           CApaid = CApaid + hlNewp;
           CAmra = CAmra - hlNewp;
           hlNewp = 0;
          //*** End Chg *** 05/02/17 ********************************
          //*** Beg Del *** 05/02/17 ********************************
          //***EndIf;
          //*** End Del *** 05/02/17 ********************************
          CAuser = Q1user;
          CApgm = Q1pgm;
          CAtime = dstime;
          CAdt = %dec(%char(%date(dsdate:*usa):*cymd0):7:0);

          update pca01;

          // write message for Aggregate adjustment
    
          // write error message only if it does not already exist
          //*** Beg Chg *** 05/02/17 ******************************
          //*** xxmsg = 'OK00118';
          kkAst = EOast;
          Chain kkAst ststl;
          if %found;
             xxmsg = ST7002;
          endif;
          //*** End Chg *** 05/02/17 ******************************
          Chain(n) mgky2 pmg03;
          If not %found(fdmgl5);

             // Find the next available message sequence number
             kkmgseq = *hival;
             Setgt mgky1 pmg02;
             Readpe(n) mgky pmg02;
    
             if not %eof(fdmgl6);
                xxmseq = mgmseq + 1;
             else;
                xxmseq = 1;
             endif;

             mgmseq = xxmseq;

             mgco# = CAco#;
             mgsub# = CAsub#;
             mgfnd = CAfnd;
             mgfyr = CAfyr;
             mgcase = CAcase;
             mgdcs# = CAdcs#;
             mgeob# = CAeob#;
             mgdseq = CAdseq;

             select;
              when x1msg = 'CC';
               mgsrc = 'C';
              other;
               mgsrc = 'S';
             endsl;

             clear mgcsi;
             mgmsg# = xxmsg;

             // Get the message description

             kkmsg# = mgmsg#;
             kkmiseq = *zeros;

             Chain(n) miky pmi01;
             if %found(ftmil);
                mgdesc = midesc;
             endif;

             mgrcn = xxrcn;
             mgstat = 'A';
             mgpgm = q1pgm;
             mguser = q1user;
             mgdt = %dec(%char(%date(dsdate:*usa):*cymd0):7:0);
             mgtime = dstime;
             mgipgm = q1pgm;
             mgiusr = q1user;
             mgidt = %dec(%char(%date(dsdate:*usa):*cymd0):7:0);
             mgitim = dstime;
    
             write pmg01;
    
          endif;
          //*** Beg Add *** 05/02/17 ********************************
          endif;
          //*** End Add *** 05/02/17 ********************************

          ReadE caky1 pca01;
        EndDo;

       Endsr;

