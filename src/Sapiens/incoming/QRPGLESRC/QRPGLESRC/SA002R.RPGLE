     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***************************************************************
     H/TITLE SA002R - Retrieve additional system values
     H***************************************************************
     H*  Description:                                               *
     H*                                                             *
     H*  Access System Control file record 'SY' to obtain values for*
     H*    SYFL01 (GL/AP Installed = 'Y')                           *
     H*                                                             *
     H*  Access System Control file record 'WC' to obtain values for*
     H*    SWC01 (CMS Installed = 'Y')                              *
     H*                                                             *
     H*  Called From:                                               *
     H*    Various programs                                         *
     H*                                                             *
     H*  Calls To:                                                  *
     H*                                                             *
     H*    None                                                     *
     H*                                                             *
     H*  Created:  04/14/98  C:33  Seq#:  WA095/11129               *
     H*                                                             *
     H***************************************************************
     H*  Revisions:                                                 *
     H*
     H*  06/28/11 C: 53 Project: SY255
     H*                 -Correct CMS flag
     H*                                                             *
     H*  02/02/11  C:235 Project: 114WXO                            *
     H*                  Claims Dept. Letter to Gather CMS Infor.   *
     H*                  ADD CMS INSTALL FLAG                       *
     H*                                                             *
     H***************************************************************
     H*  Indicators:                                                *
     H*                                                             *
     H*    60 - General File Access                                 *
     H***************************************************************
     H* System Control file
     FSSYLF     IF   E           K DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN ADD *** 06/28/11 **************************************
     D                 DS
     D  SYFL12                 1     10
     D  XXCMS                  2      2

     I*** END   ADD *** 06/28/11 **************************************

     C*
     C     *LIKE         DEFINE    SYKEY         KKKEY
     C     *LIKE         DEFINE    SYFL01        @@GLAP
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@GLAP                         GL/AP INST
     C********BEG ADD*****02/02/11**********************************
 2   C                   PARM                    @@CMS             1            CMS   INST
     C********END ADD*****02/02/11**********************************
     C*
     C                   MOVEL     *BLANK        @@GLAP
     C*
     C*  Access System Control file 'SY' record
     C*
     C                   MOVEL(P)  'SY'          KKKEY
     C*
     C     KKKEY         CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SYFL01        @@GLAP
     C*** BEGIN ADD *** 06/28/11 **************************************
     C     Q1PRMS        IFGT      1
     C                   MOVE      XXCMS         @@CMS
     C                   ENDIF
     C*** END   ADD *** 06/28/11 **************************************
     C                   ENDIF                                                  *IN60=*OFF
     C*** BEG DEL *** 06/28/11 **************************************
     C********BEG ADD*****02/02/11**********************************
     C***Access System Control file 'WC' record
     C********
     C********             MOVEL'WC'      KKKEY     P
     C********
     C********   KKKEY     CHAINSWC01                60
     C********
     C********   *IN60     IFEQ *OFF
     C********   Q1PRMS    IFGT 1
     C********             MOVELWCFL46    @@CMS
     C********             ENDIF
     C********             ENDIF
     C********END ADD*****02/02/11**********************************
     C*
     C                   RETURN
