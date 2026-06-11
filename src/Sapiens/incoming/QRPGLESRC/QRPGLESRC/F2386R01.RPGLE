     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*******************************************************************
     H/TITLE F2386R01 - OWCP/USH&L Physician Fee Schedule Maintenance
     H*****************************************************************
     H* CREATED ON 06/21/12 by C:277 for Project 114MFI
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  07/09/24  C:342 Project: FS688                               *
     H*                  2024 USH&L Fee Schedule                      *
     H*
     H*  03/08/13  C:277 SUPPORT: SEQ81781
     H*                  Recompile only
     H*
     H*  02/12/13  C:279 Project: SY266 Transform                     *
     H*                  Modified to update the status correctly.     *
     H*                                                               *
     H*
     H*****************************************************************
     FF2386F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMHLP     UF A E             DISK
     F                                     RENAME(PHL01:DBRCDF)
     FFMHLL1    IF   E           K DISK
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
     D  outdate        s             10
     D  outerr         s              1
     D  kkcode         s                   like(hlcode)
     D  kkmod          s                   like(hlmod )
     D  kkefdt         s                   like(hlefdt)
     D  #ofscr         c                   const(1)
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('OWCP/USH&L Physician Fee -
     D                                     Schedule Maintenance')
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
     C     HLKY          KLIST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*================================================================
     C*
      /Free
        //CHGSTS - SUBROUTINE FOR CHANGING STATUS

       BegSr Chgsts;

          Chain @@Rrn Dbrcdf;
            If %found(FMHLP);
              If HlStat = 'I';
                HlStat = 'A';
              Else;
                HlStat = 'I';
              Endif;

              HlUser = Q1User;
              HlPgm = Q1Pgm;
              HlTime= TimeTo6(SystemTime);
              HlDt  = dateto7(SystemDate);
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
           and scCode = *blanks;
           *in71 = *on;
           *in90 = *on;
         Endif;

        //DESCRIPTION CANNOT BE BLANK

          If *in90 = *off and scDsc1 = *blanks;
            *in75 = *on;
            *in90 = *on;
          Endif;

        //CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY

         kkCode = scCode;
         KkMod  = scMod ;
         kkefdt = dateto7(cvttodate(scEfdt));

         If *in40 = *on and *in90 <> *on;
           Chain Hlky phl01;
             If %Found(FMHLL1);
               *in74 = *on;
               *in90 = *on;
             Endif;
         EndIf;

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
           *in73 = *on;
           *in90 = *on;
         Endif;


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
           scCode = HlCode;
           scMod  = HlMod ;
           scEfdt = dtCheck(Hlefdt);
           //*** Beg Chg *** 07/09/24 *******************************************
           //*** scPyst = HlPyst;
           scdfec   = Hldfec;
           scdeeoic = Hldeeoic;
           scdcmwc  = Hldcmwc;
           //*** End Chg *** 07/09/24 *******************************************
           scWkrv = HlWkrv;
           scNfpe = HlNfpe;
           scFape = HlFape;
           scMlrv = HlMlrv;
           scGlob = HlGlob;
           scCf   = HlCf  ;
           scDsc1 = %Subst(HlDesc :1 :50);
           scDsc2 = %Subst(HlDesc :51 :50);
           scMult = HlMult;
           scBilA = HlBilA;
           scAsst = HlAsst;
           scCosg = HlCosg;
           scTesg = HlTesg;
           scEnba = HlEnba;
           scStat = HlStat;
         Else;
           scCode = *blanks;
           scMod  = *blanks;
           scEfdt = *zeros;
           //*** Beg Chg *** 07/09/24 *******************************************
           //*** scPyst = *blanks;
           scdfec   = *blanks;
           scdeeoic = *blanks;
           scdcmwc  = *blanks;
           //*** End Chg *** 07/09/24 *******************************************
           scWkrv = *zeros;
           scNfpe = *zeros;
           scFape = *zeros;
           scMlrv = *zeros;
           scGlob = *blanks;
           scCf   = *zeros;
           scDsc1 = *blanks;
           scDsc2 = *blanks;
           scMult = *blanks;
           scBilA = *blanks;
           scAsst = *blanks;
           scTesg = *blanks;
           scEnba = *blanks;
       // ***** Beg Chg ******* 02/12/13 ********************
           // scStat = *blanks;
           scStat = 'A';
       // ***** End Chg ******* 02/12/13 ********************
         Endif;
       EndSr;

        //MVSCDB - Move screen fields to data base fields

       BegSr Mvscdb;

         HlCode = scCode;
         HlMod  = scMod;
         Hlefdt = dateto7(cvttodate(scEfdt));
         //*** Beg Chg *** 07/09/24 *******************************************
         //*** HlPyst = scPyst;
         Hldfec   = scdfec;
         Hldeeoic = scdeeoic;
         Hldcmwc  = scdcmwc;
         //*** End Chg *** 07/09/24 *******************************************
         HlWkrv = scWkrv;
         HlNfpe = scNfpe;
         HlFape = scFape;
         HlMlrv = scMlrv;
         HlGlob = scGlob;
         HlCf   = scCf;
         HlDesc = scdsc1 + scDsc2;
         HlMult = scMult;
         HlBilA = scBilA;
         HlAsst = scAsst;
         HlCosg = scCosg;
         HlTesg = scTesg;
         HlEnba = scEnba;
         HlStat = scStat;

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

       inData = 'OWCP/USH&L Physician Fee Schedule Maintenance';
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

B01      If %found(FMHLP);
           Exsr Mvscdb;
           HlUser = Q1User;
           HlPgm  = Q1Pgm;
           HlTime= TimeTo6(SystemTime);
           HlDt  = dateto7(SystemDate);

         Update Dbrcdf;
         Endif;

       EndSr;

        //WRITER - Write data base record and after processing

       BegSr Writer;

       Clear Dbrcdf;

       Exsr Mvscdb;
       // ***** Beg Add ******* 02/12/13 ********************
       Hlstat = 'A';
       // ***** End Add ******* 02/12/13 ********************
       HlUser = Q1User;
       HlPgm  = Q1Pgm;
       HlIUsr = Q1User;
       HlTime= TimeTo6(SystemTime);
       HlItim= TimeTo6(SystemTime);
       HlDt  = dateto7(SystemDate);
       HlIdt = dateto7(SystemDate);

       Write dbrcdf;

       Feod Fmhlp;

       EndSr;
      /End-free
