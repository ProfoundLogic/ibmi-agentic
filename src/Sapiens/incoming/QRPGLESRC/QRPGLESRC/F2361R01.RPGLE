     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  REVISIONS
     H*
     H*****************************************************************
     FF2361F01  CF   E             WORKSTN
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     F*
     F*  Physical file definition (for maintaining record)
     FFMPRPFL   UF   E             DISK
     F                                     RENAME(PPR01:DBRCDF)
     FFTCCL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('FL OPPS Payment information-
     D                                     screen')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D varData         S             80
     D varLen          S              3  0
     D varCtyCd        S             10
     D*
     DAllignCenter     pr                  extpgm('SRCENTER')
     D varData                       80
     D varLen                         3  0
     D*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C                   PARM                    @@EUCN            3
     C                   PARM                    @@EOMOD           6 4
     C                   PARM                    @@CASVR           9 2
     C                   PARM                    @@CARVR           9 2
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* PARM LIST FOR CALL TO STATE SPECIFIC PROGRAMS
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      @@RRN         ##RRN             9 0
     C                   PARM      @@ST          ##ST              2
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     keyFcctp      Klist
     C                   Kfld                    PRCO#
     C                   Kfld                    varCtyCd
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    PRCO#         ##CO#
     C     *LIKE         DEFINE    PRCPT4        KKCPT4
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C*
      /Free
       /Copy SCOPYMT01C

       //================================================================
       // DLTPRE - Pre-processing before delete occurs
       //================================================================

       Begsr DLTPRE;
       Endsr;

       //================================================================
       // DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
       //================================================================

       Begsr DSPLYR;
       Endsr;

       //================================================================
       // ERRCK1 - Error checking for srceen 1 (SCR01)
       //================================================================

       Begsr ERRCK1;
       //----------------------------------------------------------------
       // CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
       //----------------------------------------------------------------

       //----------------------------------------------------------------
       // WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
       //----------------------------------------------------------------
       Endsr;

       //================================================================
       // ERRCK2 - Error checking for srceen 2 (SCR02)
       //================================================================
       Begsr Errck2;

       //----------------------------------------------------------------
       // WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
       //----------------------------------------------------------------

       Endsr;

       //================================================================
       // ERRCK3 - Error checking for srceen 3 (SCR03)
       //================================================================
       Begsr Errck3;

       //----------------------------------------------------------------
       // WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
       //----------------------------------------------------------------

       Endsr;

       //================================================================
       // MVDBSC - Move data base fields to screen fields
       //================================================================
       Begsr MVDBSC;

        If *in40 = *off;

           SCCPT4 = PRCPT4;
           SCEFDT = %dec(%char(%date(PREFDT:*cymd):*usa0):8:0);
           SCDSC1 =  PRDSC1 + ' ' + PRDSC2;
           SCSTAT = PRSTAT;

           //----------------------------------------------------
           // Getting County code and County name.
           //----------------------------------------------------
           varCtyCd = 'FL' + @@EUCN;
           Chain keyfcctp PCc01;
              If %found(FTCCL1);
                 SCCYC = @@EUCN;
                 SCCYD = CCDESC;
              Endif;

           //----------------------------------------------------
           // Getting Base rate for appendix B & C.
           //----------------------------------------------------
           indVisUnSc = *off;                                    //"Screen indicator used
           indVisSc = *off;
           If @@CASVR <> 0;
              indVisUnSc = *on;
              SCMOD = @@EOMOD;
              SCBRT = @@CASVR;
              SCATP = @@EOMOD * @@CASVR;
           Else;
              indVisUnSc = *off;
           Endif;

           If @@CARVR <> 0;
              indVisSc = *on;
              SCMOD1 = @@EOMOD;
              SCCRT  = @@CARVR;
              SCBTP  = @@EOMOD * @@CARVR;
           Else;
              indVisSc = *off;
           Endif;

        Else;

           SCCPT4 =  *BLANKS;
           SCEFDT =  *ZEROS;
           SCDSC1 =  *BLANK;
           SCSTAT =  'A';
           SCCYC  =  *zeros;
           SCCYD  =  *blanks;
           SCMOD  =  *zeros;
           SCMOD1 =  *zeros;
           SCBRT  =  *zeros;
           SCCRT  =  *zeros;
           SCATP  =  *zeros;
           SCBTP  =  *zeros;

        Endif;

       Endsr;
       //================================================================
       // MVSCDB - Move screen fields to data base fields
       //================================================================
       Begsr MVSCDB;
       Endsr;

       //================================================================
       // OTHERR - Processing for other modes
       //================================================================
       Begsr OTHERR;
       Endsr;

       //================================================================
       // OTHFKS - Processing for other functions keys
       //================================================================
       Begsr OTHFKS;
       Endsr;

       //================================================================
       // PGMEND - End of program processing
       //================================================================
       Begsr PGMEND;
         *inlr = *on;
       Endsr;

       //================================================================
       // PGMSTR - Start of program processing
       //================================================================
       Begsr PGMSTR;

       *in09 = *off;
       *in40 = *off;
       If @@MODE = 'D';                                           //Inquiry mode.
          *in09 = *on;
       Endif;

       Chain @@rrn DBRCDF;
         If %found();
            *in99 = *off;
         Endif;

       ##DATA = OPTNM;
       varLen = 40;
       AllignCenter(vardata: varlen);

       SCprds = varData;
       DSpTyp = *on;

       Endsr;

       //================================================================
       // PREOTH - Pre-processing for other modes
       //================================================================
       Begsr PREOTH;
       Endsr;

       //================================================================
       // UPDATR - Subroutine to update file.
       //================================================================
       Begsr UPDATR;
       Endsr;

       //================================================================
       // WRITER - Write data base record and after processing
       //================================================================
       Begsr WRITER;
       Endsr;
