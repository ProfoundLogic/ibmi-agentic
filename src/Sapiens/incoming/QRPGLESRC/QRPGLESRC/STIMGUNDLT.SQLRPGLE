     H Copyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H BNDDIR('STBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     H*
     H/TITLE Undelete Image
     H*********************************************************************
    H*  DESCRIPTION:  This program prompts Deleted Image IDs to be restor*
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    MENU     : Image Setup >> Undelete Images                      *
    H*                                                                   *
    H*                                                                   *
    H*  PROGRAMMER: C:340                                                *
    H*  CREATED:    05/28/24            PROJECT: 152WCA                  *
     H*********************************************************************
     H* REVISIONS:                                                        *
     H*                                                                   *
     H*                                                                   *
     H*********************************************************************
     H* INDICATORS:
     H*********************************************************************
     * Files Declaration                                                 *
     fSTIMGUNDLTcf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflDImgInf:RrnDImgInf)
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdComPnl)
    f*** Index Entries File
     fIMIEP     UF   E             Disk    ReName(PIE01:PIE0P)
    f*** Index Entries File
     fIMIEL5    IF   E           K Disk    infds(dbfds)
     f
    f*** Index Queue Description File
     fIMIQL     IF   E           K Disk
     f
    f*** Document Object Transaction
     fIMDOL1    UF   E           K Disk
     ***********************************************************
     * Copybook having Prototypes.
     ***********************************************************
     d
     d/copy SPrDate
     d/copy sPrMask
     d/copy SprModule
     d/copy SprSql
     d/copy sPrCompany
     d/copy sPrUsrprms
     d/copy sPrString
     d/copy sprGenPop
     d
    d* Validate Company Addition info. procedure.
     d ValidateCmpny   pr              n
     d inMode                         1    const
     d
     d* Undelete Images
     d PrcUnDelete     pr
     d inRrn                          9  0

     d* Carrier Grid processing procedure.
     d LoadCarrGrid    pr
     d inSflrrn                       9  0

     d* Carrier detail box loading procedure.
     d PrcCarDtl       pr
     d inSflrrn                       9  0
     d
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d
     d savDCID         S                   Like(IEDCID) Inz(*Blanks)
     d intCO#          S              3  0
     d wrkDcID         S                   Like(scDCID) Inz(*Blanks)

     D lo              c                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D up              c                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D dbfds           ds
     D  Imgrrn               397    400b 0
     **************************************************************************
     *                       P R O G R A M   M A I N L I N E
     **************************************************************************

       Exec Sql Set Option Commit = *None;


      // Main Procesing
       DspDelImages();

       *Inlr = *on;

       //===================================================================
       // DspDelImages Display DelImages
       //===================================================================
     p DspDelImages    b
     d DspDelImages    pi

     d  inrrn          S              9  0
     d  isexit         S               n   inz(*off)
     d  strMsg         S            100A
      /free

       // Set Action Panel
       APnlHeight = 550;
       intCo# = GetUserInfo(q1user);
       scCompName = GetCompNam(intCo#);

       Clear RcdDImgMst;
       btnImgCls = *Off;

       //*  Process until Close button is pressed.

          // Clearing the Deleted Inages subfiles.

       ExSr RefreshScr;

       Dow isexit = *off And BtnImgCls = *Off;
         // Execute record formats.
          Write rcdComPnl;
          Exfmt RcdDimgMst;
          Read rcdComPnl;

          Select;

         // Cancel Button click
          When btnImgcls = *on;
             isexit = *on;

         // Refresh/New Image ID
          When btnImgNew = *On;
             btnImgNew = *Off;
             ExSr RefreshScr;

         // Search
          When btnImgSrc = *On;
             btnImgSrc = *Off;
          clrDimgSfl = *on;
          dspDimg = *off;
          Write RcdDimgMst;
          clrDimgSfl = *off;
          dspDimg = *on;
          rrnDimgInf = 0;
                If ValidateDCid = *On;
                   LoadDimgGrid();
                Endif;

         // Other Processing
          Other;

          Dou %Eof;
             Readc SflDimgInf;
             If %eof();
                Leave;
             Endif;
            // Image Undelete Select Button
             If btnSelect  = *on;
                btnSelect  = *off;
                inrrn = scDbImgRrn;
                PrcUnDelete(inrrn);
                Write RcdDimgMst;
                strMsg = 'Image has been Undeleted and is now available in'+
                         ' the users image queue.';
                popOneBtn(strMsg :'Undelete Image':'OK');
                LoadDimgGrid();
                Leave;
             EndIf;
          EndDo;    // ImageID ReadC

          EndSl;
       Enddo;    // ExitNow = *On;

       BegSr RefreshScr;
          wrkDcID = ' ';
          scDCID = ' ';
          savDCID = ' ';
          clrDimgSfl = *on;
          dspDimg = *off;
          Write RcdDimgMst;
          clrDimgSfl = *off;
          dspDimg = *on;
          rrnDimgInf = 0;
          btnImgNew = *Off;
          btnImgCls = *off;
          btnImgSrc = *off;
          erDCid = *off;
          msDCid = ' ' ;
       EndSr;

      /End-Free
     p DspDelImages    e
       //===================================================================
      // Carrier Information Grid loading.
      //===================================================================
     p LoadDimgGrid    b
     d LoadDimgGrid    pi
      /Free

          clrDimgSfl = *on;
          dspDimg = *off;
          Write RcdDimgMst;
          clrDimgSfl = *off;
          dspDimg = *on;
          rrnDimgInf = 0;

           Setll (scDCID) PIE01;
           Reade (scDCID) PIE01;
           Dow not %Eof(IMIEL5);
               If IEDsts ='DL';
                  Chain (IEQid) PIQ01;
                      If %Found and IQFL01 = 'Y';
                         scQueue = IEQid;
                         scQDesc = IQDESC;
                         rrnDImgInf  = rrnDImgInf + 1;
                         scDbImgRrn = Imgrrn;
                         Write SflDImgInf;
                         savDCID = IEDCID;
                      EndIf;
               EndIf;
              Reade (scDCID) PIE01;
           EndDo;

      /End-Free
     p LoadDimgGrid    e

     //===================================================================
     // Carrier Addition procedure.
     //===================================================================
     p PrcUnDelete     b
     d PrcUnDelete     pi
     d  inRrn                         9  0
 
     d  isexit         s               n   inz(*off)

      // Updating Image ID to Released State

        Chain inRrn PIE0P;             // to get exclusive record lock.
        If %found(IMIEP);
           IEDsts = 'RL';
           IEStat = ' ';
           IELusr = q1user;
           IELpgm = q1pgm;
           IELTIM = %Dec(%char(%Time():*HMS0):6:0);
           IELdt  = %Dec(%Char(%Date():*ymd0):6:0);
           Update PIE0P;
        EndIf;

        Chain scDCID PDO01;             // to get exclusive record lock.
        If %found(IMDOL1);
           DODsts = 'RL';
           DOLusr = q1user;
           DOLpgm = q1pgm;
           DOLTIM = %Dec(%char(%Time():*HMS0):6:0);
           DOLdt  = %Dec(%Char(%Date():*ymd0):6:0);
           Update PDO01;
        EndIf;

     p PrcUnDelete     e

      //===================================================================
      // Validate the Deleted Image (DCid)
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateDCid     b
     dValidateDCid     pi              n

     d indError        s               n
     d indDLT          s               n
     d indUserQ        S               n

       // Intialize all the messageId and error indicators.
        wrkDcID = ' ';
        erDCid       = *off;
        msDCid       = ' ' ;

       // Image ID canot be Blank
        If scDCid   = *Blanks;
           msDCID = 'Image ID cannot be left Empty';
           erDCID = *on;
           indError = *On;
        Endif;

       // Image ID Not found
        If indError = *Off;
           Exec Sql
                Select Max(IEDCID) into :wrkDcID from IMIEL5
                Where Upper(IEDCID) = Upper(:scDCID);

           If wrkDcID = ' ';
              msDCID = 'Image ID does not exists';
              erDCID = *on;
              indError = *On;
           Else;
              scDCID = %Trim(wrkDcID);
           EndIf;
        EndIf;

       // Deleted Images not on User Queue
        If indError = *Off;
           indUserQ = *Off;
           Setll (scDCID) PIE01;
           Reade (scDCID) PIE01;
           Dow not %Eof(IMIEL5);
               Chain (IEQid) PIQ01;
               If %Found and IQFL01 = 'Y';
                  indUserQ = *On;
                  Leave;
               EndIf;
              Reade (scDCID) PIE01;
           EndDo;
           If indUserQ = *Off;
              msDCID = 'Image ID does not exists on Users Queue';
              erDCID = *on;
              indError = *On;
           EndIf;
        EndIf;

       // Image ID in Deleted State Check
        If indError = *Off;
           indDLT   = *Off;
           Setll (scDCID) PIE01;
           Reade (scDCID) PIE01;
           Dow not %Eof(IMIEL5);
              If IEDsts = 'DL';
                  Chain (IEQid) PIQ01;
                  If %Found and IQFL01 = 'Y';
                     indDLT = *On;
                     Leave;
                  EndIf;
              EndIf;
              Reade (scDCID) PIE01;
           EndDo;

           If indDLT = *Off;
              msDCID = 'Image ID is not in a Deleted State';
              erDCID = *on;
              indError = *On;
           EndIf;
        EndIf;

       // Return *on indicator when No error exists.
        If indError = *On;
           Return *Off;
        Else;
           Return *On;
        EndIf;

      /End-Free
     pValidateDCid     e


