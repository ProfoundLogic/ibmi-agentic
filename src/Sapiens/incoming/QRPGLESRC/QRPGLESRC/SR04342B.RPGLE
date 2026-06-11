     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')

     H/TITLE SR04342B - CA - Designated Operations Exclusion
      *===============================================================*
      * WC040342B- Designated Operations Exclusion Endorsement        *
      *                                                               *
      * Program Description:                                          *
      *                                                               *
      *   Determines if this endorsement form should be attached to   *
      *   the policy and creates data in the Endorsement Form         *
      *   Workfile (WXFMP) prior to a form merge.                     *
      *                                                               *
      * Created   : 04/28/18                                          *
      * Programmer: C:33                                              *
      * Project   : WC2603                                            *
      *                                                               *
      *===============================================================*
      * REVISIONS:                                                    *
      *                                                               *
      * 10/28/22 C:333 Project: WC2834 (Recompile)                    *
      *                Endorsement Recognition of Cancellation        *
      *                                                               *
      * 02/07/19 C:291 Project WC2636                                 *
      *                Add monitor on class code field to prevent     *
      *                policy page blowing up from old records as     *
      *                screen field class code was changed from 40A   *
      *                to 5,0.                                        *
      *                Add Monitor on Eff/Exp Date.                   *
      *                                                               *
      * 01/09/19 C:291 Project WC2636                                 *
      *                Fix Inception date and Expiration date format  *
      *                to YYMMDD.                                     *
      *                                                               *
      * 10/10/18 C:291 Project WC2636                                 *
      *                Add processing to write endorsement data to    *
      *                the WXPEP reporting file for CA state.         *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
      * Notes:                                                        *
      *                                                               *
      *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      *                                                               *
      *  QQMODE = 1 - Called to determine if form should be attached. *
      *  QQMODE = 2 - Called to print endorsement.                    *
      *  QQMODE = 3 - Called for additional input.                    *
      *  QQMODE = 4 - Called for mid-term processing.                 *
      *===============================================================*
      ** Copybook F specs
     F/COPY SCOPYEN01F
     FWledl     if   e           k disk    Prefix(fm2:2)
     F                                     Rename(ped01:ped00)
      *** Begin Add  ***** 10/10/18 *********************************************
      * WCPOLS Endorsement transaction
     Fwxpel     uf a e           k disk
     Fwxpel1    if   e           k disk
     F                                     rename(ppe01:ppe02)
     FWmpxl1    IF   E           K DISK    rename(ppx01:ppx01)
     FWmprl1    IF   E           K DISK    rename(ppr01:ppr01)
      *** End   Add  ***** 10/10/18 *********************************************
      *=========================================================================
      *  Arrays
      *=========================================================================
      *
      *=========================================================================
      *  /COPY required constants
      *=========================================================================
      ** Form ID
     D qqForm          s             15a   inz('WC040342B ')

      ** Additional entry program
     D ADDPGM          C                   CONST('SR04342BI ')

      *** Begin Add  ***** 10/10/18 *********************************************
     D #1pgm           s             10a   INZ('W1054R')
     D #1flag          s              1a
     D intSerial#      s              2  0 inz(0)
     D strRecType      s              2a
     D strPayDesc      s                   like(PXdesc)
     D xxPxSt          s                   like(RSJrSt)
     D xxunid          s                   like(peunid)
     D x1end#          s                   like(peend#)
     D xxClsNo         s              5  0 inz(0)
     D xxClsExt        s              5a   inz(' ')
     D flgFirstWrt     s               n   inz(*On)

     D/COPY SCOPYEN01D
     D/COPY WWCPOLDAT4
      *** End   Add  ***** 10/10/18 *********************************************

     *=========================================================================
     *  PLIST definitions
     *=========================================================================

      *entry parameters
     D SR04342B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR04342B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      *** Beg Add ***** 10/10/18 *******************************
     d* Check Pgm
     d SrChkPgm        pr                  ExtPgm('SRCHKPGM')
     d                               10
     d                                1
      *** End Add ***** 10/10/18 *******************************
    
     C     keyWledl      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxFrm1
      *=========================================================================
      *  KLIST definitions
      *=========================================================================
     C     FMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRM1

      *** Beg Add  ***** 10/10/18 *********************************************
     c     emky1         klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv

      ** WXPEL - Policy page endorsement transaction file
     C     pekey         KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxdt              7 0

     C     pekey1        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr

     C     pekey2        KList
     C                   KFld                    xxco#
     C                   KFld                    xxfnd
     C                   KFld                    xxemp#
     C                   KFld                    xxfyr
     C                   KFld                    xxunid
     C                   KFld                    x1end#
    
      *** End Add ***** 10/10/18 *********************************************
    
      *=========================================================================
      *  Main
      *=========================================================================
      ** Copybook C specs
     C/COPY SCOPYEN01C
      *
      *=========================================================================
      *  OthInf - Subroutine to load form specific information & write record
      *=========================================================================
     C     OthInf        Begsr
      *
     C                   Movel(P)  XXFORM        XXFRM1           15

      /Free
       WriteInCopybook = *Off;

       Setll keyWledl ped00;
       Reade keyWledl ped00;

          Dow Not %Eof(wledl);
          FMc401 =  FM2c401;                   //Operation Name
          FMc402 =  FM2c402;                   //Standard Classification Number
          FMc403 =  FM2c406;                   //Abbreviated Phraseology
          FMFL01  = FM2FL01;                   //Check box 1
          FMFL02  = FM2FL02;                   //Check box 2
          FMc404 =  FM2c404;                   //Insured Name
          FMc405 =  FM2c405;                   //Insurer
          FMc301 =  FM2c301;                   //Policy Number
          FMDt01 =  FM2Dt01;                   //Policy Eff date
          FMDt02 =  FM2Dt02;                   //Policy Exp date
          //*** Beg Add ***** 10/10/18 *************************
          FMc407 =  FM2c407;                   //Class code Extension
          //*** Beg End ***** 10/10/18 *************************

         // FMead1  = FM2c403;                   //Address 1

         // FMead2  = ' ' ;                      //Address 2
         // FMead2 = %Trim(FM2c201) + ', ' +
         // %Trim(FM2c101) + '  ' +
         // %Trim(FM2c102) + '-' +
         // %Trim(FM2c103);

          // Write the output record in WXFMP.
          Exsr WriteWorkFile;

          //*** Beg Add ***** 10/10/18 *************************
          Clear e04342DM;
          Clear e04342DO;

          intSerial# = intSerial# + 1;
          //Write DM record
          strRecType = 'DM';
          xmNamOp  = FMc401;
          xmClsCd  = FMc402;
          xmClsWSf = FMc407;
          Exsr SetDescription;
          xmClsWrd = strPayDesc;
          xmEndSeq# = '01';
          xmEndSer# = %editc(intSerial#:'X');
          ExSr wrtWXPep;

          //Write DO record
          strRecType = 'DO';
          xoINam    = FMc404;
          xoIRCod   = '00000';
          xoIRNam   = FMc405;
          xoAltPol# = FMc301;
          //*** Beg chg ***** 01/09/19 *******************************
          //xoAltInDt = %dec(%char(%date(FMDt01:*usa):*mdy0):6:0);
          //xoAltExDt = %dec(%char(%date(FMDt02:*usa):*mdy0):6:0);
          //xoAltInDt = %dec(%char(%date(FMDt01:*usa):*ymd0):6:0);
          //xoAltExDt = %dec(%char(%date(FMDt02:*usa):*ymd0):6:0);
          //*** Beg Chg ***** 02/07/19 *********************************
          Monitor;
             xoAltInDt = %dec(%char(%date(FMDt01:*usa):*ymd0):6:0);
          On-Error;
             xoAltInDt = 0;
          EndMon;
          Monitor;
             xoAltExDt = %dec(%char(%date(FMDt02:*usa):*ymd0):6:0);
          On-Error;
             xoAltExDt = 0;
          EndMon;
          //*** End Chg ***** 02/07/19 *********************************
          //*** End chg ***** 01/09/19 *******************************
          xoLawInsI = FMfl02;
          xoWrtAfoI = FMfl01;
          xoEndSeq# = '01';
          xoEndSer# = xmEndSer#;          //DO record should have same serial# as DM
          ExSr wrtWXPep;
          //*** End Add ***** 10/10/18 *************************

          Reade keyWledl ped00;
          EndDo;
      /End-Free
     C*
      *
     C                   Endsr

      *** Beg Add  ******* 10/10/18 *********************************************
     *=========================================================================
     *  wrtWXPep - Update Endorsement Reporting File for WCPOLS
     *=========================================================================
      /free
       Begsr wrtWXPep;

          //If printing a policy page.  (Not called from W1054R)
          SrChkPgm(#1pgm:#1flag);
          If #1flag = '0';
             //Get latest UNID
             xxdt = *hival;
             Setgt pekey ppe02;
             Readpe(n) pekey1 ppe02;
             If not %eof;
                xxunid = PEunid;
             Endif;

             //Update the endorsement reporting file with the proper data.
             x1end# = %trim(qqform);
             Chain pekey2 ppe01;          //Chain for retrieval
             If %found;
                Clear PEData;
                peuser = q1user;
                pepgm  = q1pgm;
                petime = %Dec(%Char(%Time():*iso0):6:0);
                pedt   = %Dec(%Char(%Date():*cymd0):7:0);
                Select;
                When strRecType = 'DM' and flgFirstWrt = *on;
                   PEStat = 'M';             //Update original record w/ 1st DM data - 1 time only
                   PEData = e04342DM;
                   Update ppe01;
                   flgFirstWrt = *off;
                When strRecType = 'DM' and flgFirstWrt = *off;
                   PEStat = 'M';             //Write DM records for additional class codes
                   PEData = e04342DM;
                   Write ppe01;
                   flgFirstWrt = *off;
                When strRecType = 'DO';
                   PEStat = 'O';             //DO record
                   PEData = e04342DO;
                   Write ppe01;
                Endsl;
             Endif;
          Endif;
       Endsr;

       //===================================================================
       // Set Description
       //===================================================================
       BegSr SetDescription;

          Clear strPayDesc;
          Chain rsky prs01;

          //*** Beg Chg ***** 02/07/19 *********************************
          //xxClsNo = %int(FM2c402);
          Monitor;
             xxClsNo = %int(FM2c402);
          On-Error;
             xxClsNo = 0;
          EndMon;
          //*** End Chg ***** 02/07/19 *********************************
          If FM2c407 <> *blanks;       //Class code extension
             Clear xxPxSt;
             xxClsExt = FM2c407;
             Setgt (elCo#:xxClsNo:xxClsExt:xxPxSt) PPX01;
             Readpe (elCo#:xxClsNo:xxClsExt:xxPxSt) PPX01;
             If %found;
                strPayDesc = PXDesc;
             Else;
                xxPxSt = RSJrSt;
                Setgt (elCo#:xxClsNo:xxClsExt:xxPxSt) PPX01;
                Readpe (elCo#:xxClsNo:xxClsExt:xxPxSt) PPX01;
                If %found;
                   strPayDesc = PXDesc;
                Endif;
            Endif;
          Else;
             Setgt (elCo#:ELRtCd:xxClsNo:ELpfDt) PPR01;
             Readpe (elCo#:ELRtCd:xxClsNo:ELpfDt) PPR01;
             If %found;
                strPayDesc = PRDesc;
             Endif;
          Endif;

       EndSr;
      *** End Add  ****** 10/10/18 *********************************************

      *=========================================================================
      *  PGMSTR - Beginning of program processing
      *=========================================================================
     C     PgmStr        Begsr
      *
      *
     C                   Endsr
      *=========================================================================
      *  PGMEND - End of program processing
      *=========================================================================
     C     PgmEnd        Begsr
      *
      *
     C                   Endsr
      *=========================================================================
      *  VALID - Subroutine to attach this form to the policy
      *=========================================================================
     C     VALID         BEGSR
      *
      *
     C                   ENDSR
