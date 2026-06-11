     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h/title WTCLMFROIP - FROI Display Process
     h dftname(WTCLMFROIP)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h BndDir('WTBNDDIR':'YAJL') DecEdit('0.')
     h**********************************************************************
     h*
     h*  Description: Called to Display FROI.
     h*
     h*  Called from: WTCLMFROI
     h*  Calls to:
     h*
      //*********************************************************************
      //==============================================================*
      //  Created   : 07/20/22                                         *
      //  Programmer: C:333                                            *
      //  Project   : WC2824- FROI Intake CS Standards                 *
      //===============================================================*

      * FROI Work File
     fWHFRP     if   e             disk

     d @@rrn           s              9  0
     d InJson          s               a   len(8500000) varying inz
     d OutJson         s               a   len(8500000) varying inz
     d xxOutJson       s               a   len(8500000) varying inz
     d ioRtnCode       s             10A   inz
     d PrintFROI       s            256a   inz('PrintFROI')
     d list            s                   Like(yajl_val)
     d node            s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d docNode         s                   Like(yajl_val)
     d ##Path          s            250a   varying
     d xxErrMsg        s            250a   varying
     d errMsg500       s            500a   Varying Inz(' ')
     d xxUrl           s            720a
     d ##UrlMsg        s            256a
     d ##strURL        s            720
     d strUrlTabName   s             50    inz('FROI')
     d i               s             10i 0

     d PSDS          ESDS                  extname(SZQ1P)

     d WIFROI          pr                  ExtPgm('WIFROI')
     d                              256
     d                               10
     d                                 a   len(8500000) varying
     d                                 a   len(8500000) varying
     d                               10


     d/copy sPrDate
     d/copy sPrAtrium
     d/copy sprUsrPrms
     d/copy sPrGenPop
      /include yajl_h

      // Entry of Program.
     c     *entry        plist
     c                   parm                    @@rrn

      /Free

       Chain @@rrn PFR01;
       If %found;
          Injson = *Blanks;
          Injson = '{   +
             "keyInfo": { +
                "company":"' + %EditC(FRco:'X') + '", +
                "claimantSSN":"' + FRss + '", +
                "accidentDate":"' + Fracdt + '", +
                "initialDate":"' + FRidt + '", +
                "initialTime":"' + %EditC(FRitm:'X') + '"  +
              } }' ;

          ioRtnCode = 'GOFroiPrnt';
          // calling WIFROI - Web-services FROI detail
          WIFROI(PrintFROI:Q1user:InJson:OutJson:ioRtnCode);

          xxOutJson ='{' + %trim(OutJson) +'}';       // Formating OutJson for extraction.

          If ioRtnCode <> *blanks;                                      // Error

             docNode = YAJL_string_load_tree(xxOutJson: errMsg500);

             If errMsg500 = ' ';
                list = YAJL_object_find(docNode: 'errorMsg');
                i = 0;
                Dow YAJL_array_loop(list: i : node);
                   xxErrMsg = yajl_get_string(node);
                Enddo;

                PopOneBtn(xxErrMsg:'Error');
             Endif;

          Else;                                                         // Extracting OutJson

             docNode = YAJL_string_load_tree(xxOutJson: errMsg500);

             If errMsg500 = ' ';
                node = YAJL_object_find(docNode:'documentInfo');
                node1= YAJL_object_find(node:'documentPath');
                ##Path   = YAJL_get_string(node1);

                xxUrl    =  BuildUNCUrl(##Path);                     //Download file from UNC path

                ##strURL = strUrlTabName + xxURL;
                ##UrlMsg = 'Click to open FROI';

                LaunchNewTab(##UrlMsg:'FROIs':##strURL);                //Launch new tab

              // Free up the space by removing the tree
                YAJL_tree_free(docNode);

             Endif;
          Endif;
       Endif;
       *inlr = *On;

      /End-Free
