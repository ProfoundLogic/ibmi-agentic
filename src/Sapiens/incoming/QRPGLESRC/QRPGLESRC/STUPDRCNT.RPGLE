     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE STUPDRCNT -  Update recent items
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO)
     H BNDDIR('STBNDDIR')
     h**********************************************************************
    *===============================================================*
    *  Created   : 10/16/12                                         *
    *  Programmer: C:191                                            *
    *  Project   : WC2116                                           *
    *===============================================================*
    *  Revisions:                                                   *
    *                                                               *
      * 07/06/16  C:33  Project: WC2405                               *
    *                 Purge records for user changed, not user      *
    *                  changing.                                    *
    *                                                               *
      *    09/09/14 C:279 Support: WA099 85602                        *
      *                   Added group authority for recent items.     *
      *                                                               *
    *    06/10/14 C:279 Project: 114WNK                             *
    *                   Modified to check user maximum recent item  *
    *                   count.                                      *
    *                                                               *
    *    02/06/14 C:190 Project SY336 Priss#21                      *
    *                   - Fixed duplication of recent items at      *
    *                     Home Page.                                *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     F*  Physical file definition (for maintaining record)
     Fsdrcl     uf a e           k disk    rename(prc01:prc00)
     Fsdrcl1    uf   e           k disk
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
      ********** beg add ******** 06/10/14 *******************************
     fsmupl     if   e           k disk    rename(pup01:pup00)
      ********** end add ******** 06/10/14 *******************************
     f***  Begin Add  ***  07/06/16  **************************************
     fsmupp     if   e             disk    rename(pup01:pup0p)
     f***  End   Add  ***  07/06/16  **************************************

      /copy sprDate
      ******** beg add ******* 09/09/14 *************************************
     d/copy wPrGroup
      ******** end add ******* 09/09/14 *************************************
     d*Entry
     d UpdateRecent    pr                  extpgm('STUPDRCNT')
     d  @@rrn                         9
     d  @@type                        1
     d  @@Qte                         1
     d  @@mode                        1    options(*nopass)
     d  @@modeDesc                   15    options(*nopass)
     d UpdateRecent    pi
     d  @@rrn                         9
     d  @@type                        1
     d  @@Qte                         1
     d  @@mode                        1    options(*nopass)
     d  @@modeDesc                   15    options(*nopass)

     d psds          esds                  extname(szq1p)

     D usrid           s                   like(rcusr)
     D type            s              1
     D mode            s              1
     D modeDesc        s             15
     D key             s                   like(rckey)
     D intRRN          s              9  0
     D count           s              5  0
      ********** beg add ******** 06/10/14 *******************************
     d intMxRi         s              2  0
      ********** end add ******** 06/10/14 *******************************
      ****** beg add ******* 09/09/14 ******************************
     d charGrpAccess   s              1    Inz
     d intCo#          s              3  0
     d intFnd          s              3  0
     d isNotAuth       s               n
      ****** end add ******* 09/09/14 ******************************

     D maxHistory      c                    const(8)

     D                 ds
     D  claimKey               1     40
     D  clmCo#                 1      3  0
     D  clmGroup               4      6  0
     D  year                   7      9  0
     D  case#                 10     16  0

     D                 ds
     D  employerKey            1     40
     D  empCo#                 1      3  0
     D  empGroup               4      6  0
     D  employer#              7     15  0
     D  division              16     20  0
     D  quote                 40     40

      /free
        Monitor;
         intRRN = %int(@@RRN);
        On-Error;
         intRRn =0;
        EndMon;
        key = *blanks;
        usrid = q1user;
        If %parms >=4;
          mode = @@mode;
        Else;
          mode = 'C';
        Endif;
    /  //** Begin Add *** 02/06/14 ****************************************
        If mode <> 'C' And mode <> 'D';
           mode = 'C';
        Endif;
    /  //** End   Add *** 02/06/14 ****************************************

        If %parms >=5;
          modeDesc = @@modeDesc;
        Else;
          modeDesc = *blanks;
        Endif;

        //****** beg add ******** 06/10/14 *******************************
        Chain usrId pup00;
        If %found;
           intMxRi = UPmxri;
        Endif;
        //****** end add ******** 06/10/14 *******************************

        Select;
        When @@type = 'A';
          Exsr processAgent;
        When @@type = 'C';
          Exsr processClaim;
        When @@type = 'E';
          Exsr processEmployer;
        //****** beg add ******** 06/10/14 *******************************
        When @@type = 'P';
          //***  Begin Add  ***  07/06/16  ********************************
          Clear UPusr;
          Clear UPmxRi;
          Chain(n) intRRN SMUPP;
          usrId = UPusr;
          intMxRi = UPmxRi;
          //***  End   Add  ***  07/06/16  ********************************
          Exsr PurgeRecords;
        //****** end add ******** 06/10/14 *******************************
        EndSl;
        If key <> *blanks;
          Exsr UpdateFile;
        Endif;

        eval *inlr = *On;
        Return;
        //*********************************************************
        //* Process Agent record
        //*********************************************************
        BegSr processAgent;
        EndSr;
        //*********************************************************
        //* Process Claim record
        //*********************************************************
        BegSr processClaim;
          Chain(n) intRRN pcm0p;
          clmCo#=CMco#;
          clmGroup=CMfnd;
          year=CMfYr;
          case#=CMcase;
          key = claimKey;
          type = 'C';
          //****** beg add ******** 09/09/14 ********************************
          intCo# = CMco#;
          intFnd = CMFnd;
          //****** end add ******** 09/09/14 ********************************
        EndSr;
        //*********************************************************
        //* Process Employer Record
        //*********************************************************
        BegSr processEmployer;
          Chain(n) intRRN pel0p;
          empCo#=ELco#;
          empGroup=ELfnd;
          employer#=ELemp#;
          division=ELdiv;
          quote=@@Qte;
          key = employerKey;
          type = 'E';
          //****** beg add ******** 09/09/14 ********************************
          intCo# = ELco#;
          intFnd = ELFnd;
          //****** end add ******** 09/09/14 ********************************
        EndSr;
        //*********************************************************
        //* Process Recent recprd
        //*********************************************************
        BegSr updateFile;
          Chain (usrid:key:mode) prc00;
          If %Found(sdrcl);
            If RCmode <> 'C' And RCmode <> 'D';
              RCmode = 'C';
            Endif;
            RCacdt = Dateto7(systemDate());
            RCactm = Timeto6(SystemTime());
            Update prc00;
          Else;
            //****** beg add ******** 09/09/14 ********************************
            charGrpAccess = ' ';
            isNotAuth     = *on;

            If GroupAuth(intCo#:intFnd:usrId:charGrpAccess) = '1';
               isNotAuth = *off;
               If charGrpAccess = 'I';
                  Chain(n) (usrid:key) prc00;
                  If %found(sdrcl);
                     isNotAuth = *on;
                  Endif;
               Endif;
            Endif;

            If isNotAuth = *off;
            //****** end add ******** 09/09/14 ********************************
            RCusr = usrid;
            RCkey = key;
            RCtype = type;
            RCmode = mode;
            RCmdsc = modeDesc;
            If RCmdsc = *Blanks;
              Select;
              //When RCmode = 'A';
              //  RCmdsc = 'Add';
              When RCmode = 'C';
                RCmdsc = 'Update';
              When RCmode = 'D';
                RCmdsc = 'Inquiry';
              Other;
                RCmode = 'C';
                RCmdsc = 'Update';
              EndSl;
            Endif;
            RCacdt = Dateto7(systemDate());
            RCactm = Timeto6(SystemTime());
            Write prc00;
            Exsr PurgeRecords;
            //****** beg add ******** 09/09/14 ********************************
            Endif;
            //****** end add ******** 09/09/14 ********************************
          Endif;
        EndSr;
        //*********************************************************
        //* Purge old records
        //*********************************************************
        BegSr PurgeRecords;
          count = 0;
          Setgt usrid prc01;
          DoU %Eof(sdrcl1);
            Readpe usrid prc01;
            If %Eof(sdrcl1);
              Leave;
            Endif;
            count = count + 1;
            //******* beg chg ******** 06/10/14 *******************
            // If count > maxHistory;

            If (intMxRi = 0 and count > maxHistory) or
             (intMxRi > 0 and count > intMxRi);
            //******* end chg ******** 06/10/14 *******************
              Delete prc01;
            Endif;
          Enddo;
        EndSr;
      /end-free
