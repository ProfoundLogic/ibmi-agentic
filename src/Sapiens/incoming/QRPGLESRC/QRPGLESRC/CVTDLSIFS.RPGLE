    Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
    H StoneRiver is a trademark of StoneRiver, Inc.')
    H*
    H/TITLE CVTDLSIFS
    H**************************************************************************************
    H*                                                                                    *
    H*  Description:  Convert QDLS to IFS                                                 *
    H*                                                                                    *
    H*  Created: 11/01/12   Programmer C:280   Project: c25wsj
    H*                                                                                    *
    H**************************************************************************************
    H*  Revisions:                                                                        *
    H*                                                                                    *
    H*  02/24/16  C:33  39823
     H*                  Recurrsively check folder path for existence (no comments)
    H**************************************************************************************
    F*
     FQSYSPRT   O    F  132        printer oflind(*inof)
     F                                     infds(prtfds)
    F*

     D prtFDs          ds
     D curPage               367    368I 0
     D curLine               369    372I 0

    D*
    D* Work Fields
    D*
     Derr              s              1  0
     Derror            s             45
     Dfmdoc            s             80
     Dtodoc            s             80
     Dfolder           s             40
     Dobjname          s             50
     Dobjtype          s              7
     Dflr              s             80
     Drtnflg           s              1
     Duncn             s            255
     Dfsf              s              1
     Daut              s              9
     Ddlo              s             12
     Dnwflr            s             12
     Ddesc             s             50
     Dauth             s              8
     Dinflr            s             80
     Dinfolder         s             80
     Dinfldr           s             12
     DisDeleted        s               n
     Dinfold           s             80
     Dfmdock           s             12
     Dtodock           s             12
     Dfmfolder         s             80
     Dtofolder         s             80
     Dreplace          s              4
     Dfmuncn           s            255
     Dxxloc            s              3  0
     Dfmloc            s              3  0
     Ddocument         s             30
     Ddstime           s              6  0
     Ddsdate           s              6  0
    D*
    D* Prototypes
    D*
     DChkCrtFlr        pr
     D inFolder                      80    const
     D
     D chkobj          pr                  ExtPgm('SYCHKOBJ')
     D  objname                      50    const
     D  objtype                       7    const
     D  flr                          80
     D  aut                           8    const
     D  rtnflg                        1
     D  uncn                        255
     D  fsf                           1
    D*
     D copy            pr                  ExtPgm('SYCPYDOC')
     D  fmdock                       12
     D  fmfolder                     80
     D  todock                       12
     D  tofolder                     80
     D  replace                       4    const
     D  fmuncn                      255
     D  fsf                           1
    D*
     D crtfld          pr                  ExtPgm('SYCRTFLR')
     D  nwflr                        12    const
     D  desc                         50    const
     D  auth                          8    const
     D  inflr                        80    const
    D*
     D dltdlo          pr                  ExtPgm('SYDLTDLO')
     D  dlo                          12
     D  flr                          80
     D  uncn                        255
     D  fsf                           1
    D*
     D psds          esds                  extname(szq1p)
     D*
    D*
    D*================================================================
    D*  User defined fields
    D*================================================================
    C*
    C*
    C*================================================================
    C*  Key List
    C*================================================================
    C*
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
    C*
     C     *entry        plist
     C                   parm                    @@mode            1
     C                   parm                    @@filename       10
     C                   parm                    @@rrn             9 0
     C                   parm                    @@root           10
     C                   parm                    @@document       40
     C                   parm                    @@folder         80
    C*
    C*================================================================
    C*
    C*----------------------------------------------------------------------
    C*  Process Request
    C*----------------------------------------------------------------------
    C*
      /free

      // start processing
       dstime = %Dec(%Time(): *ISO);
       dsdate = %dec(%date():*mdy);

       Select;
       When @@mode = 'P';

         // Replace '\' with '/'
          Dou xxLoc = *zero;
             xxLoc = %scan('\':@@folder);
             If xxLoc <> *zero;
               @@folder = %replace('/':@@folder:xxLoc:1);
             Endif;
          Enddo;

         // build from document
          Document = @@document;
          fmdoc = '/QDLS/' + %trim(@@folder) + '/' + %trim(@@document);

         // build to document
          todoc = '/' + %trim(@@root) + '/' + %trim(@@folder) +
                 '/' + %trim(@@document);

         // build folder
          folder = '/' + %trim(@@root) + '/' + %trim(@@folder);

         // check from document for existence
          objname = @@document;
          objtype = '*DOC';
          flr     = @@folder;
          aut     = '*ALL';
          rtnflg  = *blanks;
          uncn    = fmdoc;
          fsf = 'Q';
          CHKOBJ(objname:objtype:flr:aut:rtnflg:uncn:fsf);
         // from document not found Error
          If rtnflg = '0';
             err = 1;
             exsr writeerror;
             Return;
          Endif;

         // check to document for existence
          objname = @@document;
          objtype = '*DOC';
          flr     = @@folder;
          aut     = '*ALL';
          rtnflg  = *blanks;
          uncn = todoc;
          fsf = 'I';
          CHKOBJ(objname:objtype:flr:aut:rtnflg:uncn:fsf);
         // to document exists error
          If rtnflg = '1';
             err = 2;
             exsr writeerror;
             Return;
          Endif;

         // check to folder for existence, create if needed
          objtype = '*FLR';
          flr     = *blanks;
          aut     = '*ALL';
          rtnflg  = *blanks;
          uncn    = *blanks;
          fsf     = 'I';
          ChkCrtFlr(@@folder);

         // copy document
          fmdock   = @@document;
          fmfolder = @@folder;
          todock   = @@document;
          tofolder = @@folder;
          replace  = '*YES';
          fmuncn   = fmdoc;
          fsf      = 'I';
          COPY(fmdock:fmfolder:todock:tofolder:replace:fmuncn:fsf);

         // verify to document for existence
          objname = @@document;
          objtype = '*DOC';
          flr     = @@folder;
          aut     = '*ALL';
          rtnflg  = *blanks;
          uncn    = todoc;
          fsf     = 'I';
          CHKOBJ(objname:objtype:flr:aut:rtnflg:uncn:fsf);
         // to document not found Error
          If rtnflg = '0';
             err = 4;
             exsr writeerror;
             Return;
          Endif;

         // delete from document
          dlo  = @@document;
          flr  = @@folder;
          uncn = fmdoc;
          fsf  = 'I';
          Monitor;
             isDeleted = *off;
             DLTDLO(dlo:flr:uncn:fsf);
         // delete error
          on-Error;
             isDeleted = *on;
             err = 5;
             exsr writeerror;
             Return;
          EndMon;

          err = 7;
          exsr writeerror;
          return;

       When @@mode = 'E';
          *inlr = *on;

       Endsl;

      /end-free

    C*----------------------------------------------------------------------
    C*  Error Subroutine
    C*----------------------------------------------------------------------

      /free

       BegSr WriteError;


      // Error Description

          Select;

             When Err = 1;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                error = 'Source document not found';
             When Err = 2;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                Error = 'Destination document already exist';
             When Err = 3;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                Error = 'Destination folder not created';
             When Err = 4;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                Error = 'Document copy failed';
             When Err = 5;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                Error = 'Destination document verification failed';
             When Err = 6;
                folder = '/' + %trim('QDLS') + '/' + %trim(@@folder);
                Error = 'Source document delete failed';
             When Err = 7;
                Error = 'Successfull Move';

          Endsl;

      // Print Error

          If *inof or curLine > 99999;
            Except Header;
            *inof = *off;
          Endif;

            Except Detail;

       EndSr;

      /end-free
     O*
     O**********************************************************************
     O*
     Oqsysprt   e            header         1  3
     O                                            5 'Time:'
     O                       dstime              14 '  :  :  '
     O                                           51 'Convert QDLS to IFS'
     O                                          122 'Date:'
     O                       dsdate        y    131
     O*
     O          e            header         2
     O                                            4 'Pgm:'
     O                       q1pgm               15
     O                                          122 'Page:'
     O                       page          z    131
     O*
     O          e            header         1
     O                                            7 'File'
     O                                           18 'Record'
     O                                           39 'Document'
     O                                           70 'Folder'
     O                                          114 'Message'
     O*
     O          e            header         2
     O                                            7 'Name'
     O                                           18 'Number'
     O                                           37 'Name'
     O                                           69 'Name'
     O*
     O          e            detail         1
     O                       @@filename          10
     O                       @@rrn         z     20
     O                       document            51
     O                       folder              92
     O                       Error              128

     P*========================================================================
     P* Check for valid folder, create if not existing
     P*========================================================================
     PChkCrtFlr        b
     DChkCrtFlr        pi
     D inFolder                      80    const

     DdecLoc           s              3  0 inz
     DstrFolder        s             12    inz
     DstrDir           s             80    inz
     DstrPath          s             80    inz
     DoutPath          s             80    inz

      /Free
       strPath = inFolder;
       outPath = inFolder;
       strFolder = inFolder;

       ChkObj(outPath:objType:flr:aut:rtnFlg:uncn:fsf);
       // folder not found Error
       If rtnFlg = '0';
          Dou decLoc = *zero;
             decLoc = %scan('/':strPath:decLoc+1);
             If decLoc <> *zero;
                strDir = %subst(strPath:1:decLoc-1);
                strFolder = %subst(strPath:decLoc+1);
             Endif;
          Enddo;
          If strDir <> *blank;
             ChkCrtFlr(strDir);
          Endif;
          CrtFld(%trim(strFolder):*blanks:'*ALL':strDir);
       Endif;

      /End-free
     PChkCrtFlr        e
