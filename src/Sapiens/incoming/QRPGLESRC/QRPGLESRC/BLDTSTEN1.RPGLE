     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*********************************************************************
     H*  BLDTSTEN1 - PRINT ERROR LOG OF BUILDING TEST ENVIRONMENT     *
     H*                                                               *
     H*  DESCRIPTION:  This program Prints a listing of a file error  *
     H*                during the BLDTSTENV CL program.               *
     H*                                                               *
     H*  CREATED: 03/28/05  PROGRAMMER: C:132  PROJECT: 51WGM         *
     H*****************************************************************
     H*  REVSIONS:                                                    *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:33   Retrieve brand Name                          *
     H*                                                               *
     H*********************************************************************
     f*  AUDIT REPORT
     fqsysprt   o    f  132        printer oflind(*inob)
     f*
     DfromLib          s             10
     DFromFile         s             10
     DerrorType        s              1
     DxxMsgText        s             70    inz
     D***  Begin Add  ***  01/28/10  **************************************
     DcontactInfo      s             80    inz
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
     D
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
     d NotFound        c                   const('The File was not found in out-
     d                                     put library')
     d ErrCopy         c                   const('Error on Copy to output ibrar-
     d                                     y-Possible different fields in file')
     d ErrMenu         c                   const('Error on Reset Menu Options f-
     d                                     ile - Must CORRECT')
     d*
      *  ============================================================= *
      *
     C     *entry        plist
     C                   parm                    ErrorType
     C                   parm                    FromLib
     C                   parm                    FromFile
     C
     C                   time                    dsdttm
     C
     C                   Except    Head
     C
     C                   If        ErrorType = 'F'
     C                   movel(P)  NotFound      xxMsgText
     C                   Except    Det
     C                   endIf
     C
     C                   If        ErrorType = 'E'
     C                   movel(P)  ErrCopy       xxMsgText
     C                   Except    Det
     C                   endIf
     C
     C                   If        ErrorType = 'M'
     C                   movel(P)  ErrMenu       xxMsgText
     C                   Except    Det
     C                   endIf
     C
     C***  Begin Add  ***  01/28/10  **************************************
     C                   callp     RtvBrnd('N':brandName)
     C                   eval      contactInfo = 'Contact '
     C                                         + %trim(brandName)
     C                                         + ' support help desk'
     C                                         + ' for instructions.'
     C***  End   Add  ***  01/28/10  **************************************
     C
     C                   Except    Dets
     C
     C                   eval      *inlr = *on
     C
     oqsysprt   e            head           2 06
     o                       dstime               8 '  :  :  '
     o                                           44 'Refresh of Test Environ'
     o                                           69 'ment from Production data'
     o                       dsdate              98 '  /  /    '
     o          e            head           2
     o                       q1pgm               10
     o                       q1user              60
     o                                           93 'PAGE'
     o                       page1         3     98
     o          e            head           3
     o                                           44 'ERROR ON REFRESH OF TES'
     o                                           69 'T ENVIORONMENT FOR FILE  '
     o          e            head           1
     o                                            7 'Library'
     o                                           25 'File Name'
     o                                           50 'Error Type for file'
     o          e            head           2
     o                                            7 '-------'
     o                                           25 '---------'
     o                                           50 '-------------------'
      *
     o          e            det            4
     o                       FromLib             10
     o                       FromFile            26
     o                       xxMsgText        +   5
      *
     o          e            dets           2
     O*** BEG CHG *** 01/28/10 ****************************************
     o****                                       44 'CONTACT FISERV SUPPORT '
     o****                                       70 'HELP DESK FOR INSTRUCTIONS'
     o                       contactInfo       + 20
     O*** END CHG *** 01/28/10 ****************************************
