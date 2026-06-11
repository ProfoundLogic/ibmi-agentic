     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/title Print Error log of building 9xx test Environment
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
     H* ============================================================= *
     H*  W9401R  - PRINT ERROR LOG OF BUILDING 9xx Test ENVIRONMENT   *
     H*                                                               *
     H*  DESCRIPTION:  This program Prints a listing of a file error  *
     H*                during the W9401C CL program and/or a list of  *
     H*                Object error during the W9402C CL program.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W9401C                                                     *
     H*    W9402C                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 11/06/07  PROGRAMMER: C:132  PROJECT: 132WJL        *
     H*                                                               *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/28/10  C:249 Project: SY215
     H*            C:189 Retrieve brand Name
     H*                                                               *
     H*  01/19/10 C:198 Project: 132WJL changes                       *
     H*                 Kept the report open so that only one report  *
     H*                 is produced fro the entire run.               *
     H*                                                               *
     H* ============================================================= *
     f*  AUDIT REPORT
     f*** Beg Chg *** 01/19/10 *************************************************
     f***********qsysprt   o    f  132        printer oflind(*inob)
     fqsysprt   o    f  132        printer oflind(*inob)
     f                                     usropn
     f*** End Chg *** 01/19/10 *************************************************
     f*
     dfromLib          s             10
     dFromFile         s             10
     derrorType        s              1
     d##suffix         s              3
     dxxMsgText        s             70    inz
     d*
     d fids01        e ds                  extname(szq2p)
     d psds          esds                  extname(szq1p)
     d*
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
      *
     d* Constants
     d ErrCpyf         c                   const('Error on File Copy to Save be- "C"
     d                                     fore load 9xx libraries')
     d ErrCopy         c                   const('Error on File Copy to output - "E"
     d                                     library-Possible different fields')
     d ErrFold         c                   const('Error on Folder Copy(s) FROM - "D"
     d                                     Folders to TO Folders')
     d NotFound        c                   const('The File was not found in out- "F"
     d                                     put library')
     d ErrLibs         c                   const('Error on SAVE LIBRARY, please- "L"
     d                                      check joblog for details')
     d ErrMenu         c                   const('Error-Reset Menu Options=Inva- "M"
     d                                     lid Application Code-Please CORRECT')
     d ErrNfile        c                   const('Error-No Physical files in Li- "N"
     d                                     brary-No files to COPY')
     d ErrPcpy         c                   const('Error on Program Copy, not co- "O"
     d                                     pied - Please CHECK')
     d ErrPgms         c                   const('Error on Program Delete, not - "P"
     d                                     found - Please CHECK')
     d ErrRstr         c                   const('Error on File Restore, not fo- "R"
     d                                     und in saved library - Please CHECK')
     d ErrNrcd         c                   const('Error on File Update, NO Reco- "S"
     d                                     rds OR File does not Exist - CHECK')
     d ErrUser         c                   const('Error on SAVE USER FILE, does- "U"
     d                                      not exist - Please CORRECT')
     d*
     d ErrUnkn         c                   const('Error-Unknown type, must chec- " "
     d                                     k and if possible - Please CORRECT')
      *  ============================================================= *
      *
     D***  Begin Add  ***  01/28/10  **************************************
     DcontactInfo      s             80    inz
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *
     c     *entry        plist
     c                   parm                    ErrorType
     c                   parm                    FromLib
     c                   parm                    FromFile
     c                   parm                    ##Suffix
     c
     c                   time                    dsdttm
     c
     c*** Beg Del *** 01/19/10 *************************************************
     c*******            Except    Head
     c*** End Del *** 01/19/10 *************************************************
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        Errortype = 'A'
     c                   open      qsysprt
     c                   Except    Head
     c                   endif
     c                   if        *inob = *on
     c                   eval      *inob = *off
     c                   Except    Head
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c
     C***  Begin Add  ***  01/28/10  **************************************
     C                   callp     RtvBrnd('N':brandName)
     C                   eval      contactInfo = 'Contact '
     C                                         + %trim(brandName)
     C                                         + ' support help desk'
     C                                         + ' for instructions.'
     C***  End   Add  ***  01/28/10  **************************************
     c
     c                   select
     c
      *  Error on File Save before Copies
     c                   When      ErrorType = 'C'
     c                   movel(P)  ErrCpyf       xxMsgText
     c                   Except    Det
     c
      *  Error on File Copy
     c                   When      ErrorType = 'D'
     c                   movel(P)  ErrFold       xxMsgText
     c                   Except    Det
     c
      *  Error on File Copy
     c                   When      ErrorType = 'E'
     c                   movel(P)  ErrCopy       xxMsgText
     c                   Except    Det
     c
      * Error - File not found in Copy To Library.
     c                   When      ErrorType = 'F'
     c                   movel(P)  NotFound      xxMsgText
     c                   Except    Det
     c
      * Error - Save Library does NOT Exist.
     c                   When      ErrorType = 'L'
     c                   movel(P)  ErrLibs       xxMsgText
     c                   Except    Det
     c
      * Error - Reset Menu Options File.
     c                   When      ErrorType = 'M'
     c                   movel(P)  ErrMenu       xxMsgText
     c                   Except    Det
     c
      * Error - No Physical files in Library
     c                   When      ErrorType = 'N'
     c                   movel(P)  ErrNfile      xxMsgText
     c                   Except    Det
     c
      * Error - Program Copy not completed
     c                   When      ErrorType = 'O'
     c                   movel(P)  ErrPcpy       xxMsgText
     c                   Except    Det
     c
      * Error - Program Delete object not found
     c                   When      ErrorType = 'P'
     c                   movel(P)  ErrPgms       xxMsgText
     c                   Except    Det
     c
      * Error - File Restore object not found in Saved Library
     c                   When      ErrorType = 'R'
     c                   movel(P)  ErrRstr       xxMsgText
     c                   Except    Det
     c
      * Error - Update file has no Records in Refresh Library
     c                   When      ErrorType = 'S'
     c                   movel(P)  ErrNrcd       xxMsgText
     c                   Except    Det
     c
      * Error - Save User file - Does not exist
     c                   When      ErrorType = 'U'
     c                   movel(P)  ErrUser       xxMsgText
     c                   Except    Det
     c
      * Error - Unknown type of error
     c                   Other
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        Errortype <> 'A' and Errortype <> 'Z'
     c*** End Add *** 01/19/10 *************************************************
     c                   movel(P)  ErrUnkn       xxMsgText
     c                   Except    Det
     c*** Beg Add *** 01/19/10 *************************************************
     c                   endif
     c*** End Add *** 01/19/10 *************************************************
     c
     c                   endsl
     c
     c*** Beg Del *** 01/19/10 *************************************************
     c*****              Except    Dets
     c*** End Del *** 01/19/10 *************************************************
     c*** Beg Add *** 01/19/10 *************************************************
     c                   if        Errortype = 'Z'
     c                   Except    Dets
     c                   close     qsysprt
     c                   eval      *inlr = *on
     C                   endif
     c*
     c*** End Add *** 01/19/10 *************************************************
     c
     c*** Beg Chg *** 01/19/10 *************************************************
     c********           eval      *inlr = *on
     c                   return
     c*** End Chg *** 01/19/10 *************************************************
     c
     oqsysprt   e            head           1 06
     o                       dstime               8 '  :  :  '
     o                                           44 'Refresh of Test Environ'
     o                                           69 'ment from Production data'
     o                       dsdate              98 '  /  /    '
     o          e            head           1
     o                       q1pgm               10
     o                                           93 'PAGE'
     o                       page1         3     98
     o          e            head           1
     o                       q1user              10
     o          e            head           1
     o                                           41 'ERROR REPORT ON REFRESH OF'
     o                       ##Suffix            45
     o                                           67 ' ENVIRONMENT FOR FILE/'
     o                                           75 'LIB/OBJ '
     o          e            head           2
     o                                           45 '---'
     o          e            head           1
     o                                            7 'Library'
     o                                           25 'File/Obj '
     o                                           57 'Error Type for File or OBJ'
     o          e            head           2
     o                                            7 '-------'
     o                                           25 '---------'
     o                                           57 '--------------------------'
      *
     o          e            det            4
     o                       FromLib             10
     o                       FromFile            26
     o                       xxMsgText        +   5
      *
     o          e            dets           2
     o*** Beg Chg *****  01/28/10  ************************************
     o****                                       44 'CONTACT FISERV SUPPORT '
     o****                                       70 'HELP DESK FOR INSTRUCTIONS'
     o                       contactInfo       + 20
     o*** End Chg *****  01/28/10  ************************************
