     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE F2387R01 - OWCP/USH&L Anesthesia Conversion Factors Main.
     H*******************************************************************
     H* CREATED ON 06/21/12 by C:277 for Project 114MFI
     H*******************************************************************
     H* REVISIONS:
     H*
     H*
     H*****************************************************************
     FF2387F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTHAP     UF A E             DISK
     F                                     RENAME(PHA01:DBRCDF)
     FFTHAL1    IF   E           K DISK
     D*
     D DBFDS           DS
     D*
     DCvtToDate        pr              d
     D numDate                        8  0 Const
     D
     DDateTo7          PR             7  0
     D date                            d   Const
     D
     DdtCheck          pr             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)

     DTimeTo6          pr             6  0
     D time                            T   Const
     D*
     DsystemDate       pr              D
     D*
     DsystemTime       pr              T
     D*
     D CVTDATE         pr                  extPgm('SRCVTDAT')
     D  indatein                      8  0
     D  infromfmt                     8
     D  intofmt                       8
     D  Outdate                      10
     D  outerr                        1
     D*
     D SRCENTER        pr                  extPgm('SRCENTER')
     D  indata                       80
     D  inleng                        3  0
     D*
     D ACDPGMCal       pr                  extpgm('F2368R01')
     D  inkc                          1
     D  inkl                          1
     D  inMode                        1
     D  inRrn                         9  0
     D*
     D  indata         s             80
     D  inleng         s              3  0
     D  indatein       s              8  0
     D  infromfmt      s              8
     D  intofmt        s              8
     D  Outdate        s             10
     D  outerr         s              1
     D  kkzip          s                   like(hazip)
     D  kkst           s                   like(hast)
     D  kkefdt         s                   like(haefdt)
     D  #ofscr         c                   const(1)
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('OWCP/USH&L Anesthesia Conv. -
     D                                     Factors')
     D COMAPI          C                   CONST('FA007R')
     C/COPY SCOPYMT01C
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Search parameter list.
     C*
     C     SCHPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     HAKY          KLIST
     C                   KFLD                    KKZIP
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
     C*================================================================
      /Free
        //CHGSTS - SUBROUTINE FOR CHANGING STATUS

       BegSr Chgsts;

          Chain @@Rrn Dbrcdf;
            If %found(FTHAP);
              If HaStat = 'I';
                HaStat = 'A';
              Else;
                HaStat = 'I';
              Endif;

              HaUser = Q1User;
              HaPgm = Q1Pgm;
              HaTime= TimeTo6(SystemTime);
              HaDt  = dateto7(SystemDate);
            Update Dbrcdf;
            Endif;

       EndSr;

        //DLTPRE - Pre-processing before delete occurs

       BegSr Dltpre;
       EndSr;

        //DSPLYR - Processing after displaying information

       BegSr Dsplyr;
       EndSr;

        //ERRCK1 - Error checking for srceen 1 (SCR01)

       BegSr ErrCk1;

         If *in40= *on and *in90 <> *on
           and scZip = *blanks;
           *in71 = *on;
           *in90 = *on;
           LeaveSr;
         Endif;

        //EFFECTIVE DATE MUST BE VALID (IF IN ADD MODE)

           indatein = scEfdt;
           infromfmt = *Blanks;
           intofmt = *Blanks;
           Outdate = *Blanks;
           Outerr = *Blanks;

           callp  CVTDATE(indatein:infromfmt:intofmt:Outdate:
                  Outerr);

         If *in40= *on and *in90 <> *on
           and Outerr = '1';
           *in72 = *on;
           *in90 = *on;
           LeaveSr;
         Endif;

        //State mus be non blanks

         If *in40= *on and *in90 <> *on
           and scst = *blanks;
           *in73 = *on;
           *in90 = *on;
           LeaveSr;
         Endif;

        //MSA Number must be non blanks

         If *in40= *on and *in90 <> *on
           and scMsa= *blanks;
           *in75 = *on;
           *in90 = *on;
           LeaveSr;
         Endif;

        //CONVERSION FACTOR CANNOT BE BLANK

          If *in90 = *off and scCf = *zeros;
            *in76 = *on;
            *in90 = *on;
          Endif;

        //CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY

         kkzip = %int(sczip);
         Kkst  = scst  ;
         kkefdt = dateto7(cvttodate(scEfdt));

         If *in40 = *on and *in90 <> *on;
           Chain Haky pha01;
             If %Found(FTHAL1) and HaMsa = scMsa
               and HaCf = scCf;
               *in74 = *on;
               *in89 = *on;
               *in90 = *on;
             Endif;
         EndIf;

       EndSr;

        //ERRCK2 - Error checking for srceen 2 (SCR02)

       BegSr ErrCk2;
       EndSr;

        //ERRCK3 - Error checking for srceen 3 (SCR03)

       BegSr ErrCk3;
       EndSr;

        //KJDO - F10=SEARCH.

       BegSr Kjdo;
       EndSr;

         //MVDBSC - Move data base fields to screen fields

       BegSr Mvdbsc;

         If *in40 = *off;
           scZip  = %editc(HaZip:'X');
           scSt   = Hast  ;
           scEfdt = dtCheck(Haefdt);
           scStat = Hastat;
           scMsa  = Hamsa ;
           scCf   = Hacf  ;
         Else;
           scZip  = *blanks;
           scSt   = *blanks;
           scEfdt = *zeros;
           scStat = *blanks;
           scMsa  = *blanks;
           scCf   = *zeros;
         Endif;
       EndSr;

        //MVSCDB - Move screen fields to data base fields

       BegSr Mvscdb;

         HaZip  = %int(scZip);
         HaSt   = scSt;
         HaEfdt = dateto7(cvttodate(scEfdt));
         HaStat = scStat;
         HaMsa  = scMsa ;
         HaCf   = scCf  ;

       EndSr;

        //OTHERR - Processing for other modes

       BegSr OthErr;
       EndSr;

        //OTHFKS - Processing for other functions keys

       BegSr Othfks;

         If *Inkj = *on;
           Exsr Kjdo;
         Endif;

       EndSr;

        //PGMEND - End of program processing

       BegSr PgmEnd;

         *inlr = *on;

       EndSr;

        //PGMSTR - Start of program processing

       BegSr PgmStr;

       Select;
        //Inquiry
       When @@Mode = 'D';
       *in09 = *on;

        //Add
       When @@Mode = 'A';
       *in40 = *on;
       *in81 = *on;

       EndSl;

       inData = 'OWCP/USH&L Anesthesia Conv. Factors';
       inLeng = 40;
       callp srcenter(inData:inLeng);

       Scprds = inData;
       DspTyp = *on;

       EndSr;

        //PREOTH - Pre-processing for other modes

       BegSr Preoth;

       Select;

       When @@Mode = 'S';
       Exsr Chgsts;
       Return;

       EndSl;

       EndSr;

        //UPDATR - Update data base record and after processing

       BegSr Updatr;

       Chain @@Rrn dbrcdf;

B01      If %found(FTHAP);
           Exsr Mvscdb;
           HaUser = Q1User;
           HaPgm  = Q1Pgm;
           HaTime= TimeTo6(SystemTime);
           HaDt  = dateto7(SystemDate);

         Update Dbrcdf;
         Endif;

       EndSr;

        //WRITER - Write data base record and after processing

       BegSr Writer;

       Clear Dbrcdf;

       Exsr Mvscdb;
       HaUser = Q1User;
       HaPgm  = Q1Pgm;
       HaIUsr = Q1User;
       HaTime= TimeTo6(SystemTime);
       HaItim= TimeTo6(SystemTime);
       HaDt  = dateto7(SystemDate);
       HaIdt = dateto7(SystemDate);

       Write dbrcdf;

       Feod FTHAP;

       EndSr;
      /End-free
