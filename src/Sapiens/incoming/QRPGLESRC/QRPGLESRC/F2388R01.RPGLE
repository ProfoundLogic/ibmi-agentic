     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*******************************************************************
     H/TITLE F2388R01 - OWCP/USH&L ASC Allowable Procedures Maintenance
     H*******************************************************************
     H* CREATED ON 06/21/12 by C:277 for Project 114MFI
     H*******************************************************************
     H* REVISIONS:
     H*
     H* 03/30/17  C:212  Project: FS520
     H*                  Add field SCANCILL and related processing
     H*
     H*****************************************************************
     FF2388F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFTAAP     UF A E             DISK
     F                                     RENAME(PAA01:DBRCDF)
     FFTAAL1    IF   E           K DISK
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
     D  kkhcpc         s                   like(aahcpc)
     D  kkmod          s                   like(aamod)
     D  kkefdt         s                   like(aaefdt)
     D  #OFSCR         C                   CONST(1)
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('OWCP/USH&L ASC Allowable -
     D                                     Codes')
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
     C     AAKY          KLIST
     C                   KFLD                    KKHCPC
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEFDT
     C*================================================================
      /Free
        //CHGSTS - SUBROUTINE FOR CHANGING STATUS

       BegSr Chgsts;

          Chain @@Rrn Dbrcdf;
            If %found(FTAAP);
              If AaStat = 'I';
                AaStat = 'A';
              Else;
                AaStat = 'I';
              Endif;

              AaUser = Q1User;
              AaPgm = Q1Pgm;
              AaTime= TimeTo6(SystemTime);
              AaDt  = dateto7(SystemDate);
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
           and scHcpc = *blanks;
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
           *in73 = *on;
           *in90 = *on;
           LeaveSr;
         Endif;

        //CHECK TO BE SURE CODE HAS NOT BEEN USED PREVIOUSLY

         kkHcpc = scHcpc;
         KkMod = scMod  ;
         kkefdt = dateto7(cvttodate(scEfdt));

         If *in40 = *on and *in90 <> *on;
           Chain Aaky paa01;
             If %Found(FTAAL1) and AaMod = scMod
               and AaAdd = scAdd;
               *in74 = *on;
               *in89 = *on;
               *in90 = *on;
             Endif;
         EndIf;

       //***** Beg Add *** 03/30/17 **************************************
         // Ancillary Service must be entered in Add Mode

         If *in40= *on and *in90 <> *on
           and scAncill = *blanks;
           erAncill = *on;
           msAncill = 'MCR0503';
           *in90 = *on;
           LeaveSr;
         Endif;
       //***** End Add *** 03/30/17 **************************************

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
           scHcpc  = AaHcpc;
           scMod  = AaMod ;
           scEfdt = dtCheck(Aaefdt);
           scAdd  = AaAdd;
           scStat = AaStat;
         //***** Beg Add *** 03/30/17 **************************************
           scAncill = AaAncill;
         //***** End Add *** 03/30/17 **************************************
         Else;
           scHcpc = *blanks;
           scMod  = *blanks;
           scEfdt = *zeros;
           scAdd  = *blanks;
           scStat = *blanks;
         //***** Beg Add *** 03/30/17 **************************************
           scAncill = 'N';
         //***** End Add *** 03/30/17 **************************************
         Endif;
       EndSr;

        //MVSCDB - Move screen fields to data base fields

       BegSr Mvscdb;

         AaHcpc = scHcpc;
         AaMod  = scMod;
         AaEfdt = dateto7(cvttodate(scEfdt));
         AaStat = scStat;
         AaAdd  = scAdd ;
         //***** Beg Add *** 03/30/17 **************************************
         AaAncill = scAncill;
         //***** End Add *** 03/30/17 **************************************

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

       inData = 'OWCP/USH&L ASC Allowable Codes';
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

B01      If %found(FTAAP);
           Exsr Mvscdb;
           AaUser = Q1User;
           AaPgm  = Q1Pgm;
           AaTime= TimeTo6(SystemTime);
           AaDt  = dateto7(SystemDate);

         Update Dbrcdf;
         Endif;

       EndSr;

        //WRITER - Write data base record and after processing

       BegSr Writer;

       Clear Dbrcdf;

       Exsr Mvscdb;
       AaUser = Q1User;
       AaPgm  = Q1Pgm;
       AaIUsr = Q1User;
       AaTime= TimeTo6(SystemTime);
       AaItim= TimeTo6(SystemTime);
       AaDt  = dateto7(SystemDate);
       AaIdt = dateto7(SystemDate);

       Write dbrcdf;

       Feod FTAAP;

       EndSr;
      /End-free
