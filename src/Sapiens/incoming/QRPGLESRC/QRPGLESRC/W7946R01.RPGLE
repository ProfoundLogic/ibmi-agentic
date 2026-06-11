     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
     H*/TITLE W7946R01 - State Select - Populate Work File
    H*********************************************************************
    H*  Description: Populates WXSSELP workfile.                         *
    H*                                                                   *
    H*  Called from: W7946C                                              *
    H*                                                                   *
    H*  Calls to:                                                        *
    H*                                                                   *
    H*  Created: C:296  Project: 114MQA   Date: 12/10/14                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*                                                                                    *
    H**************************************************************************************
    f*
    f*  Physical file definition (for refreshing subfile record)
     fwxsselp   uf a e             disk
    f*  State master file
     fststp     if   e           k disk    rename(pst01:pst02)
     fststl     if   e           k disk
     fwtsselp   if   e           k disk    rename(pss01 : pss0t)

     d*-- Variables declare section --*
     d sys_date        s               d   datfmt(*usa)
     d sys_date7       s              7  0
     d psds          esds                  extname(szq1p)
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d*
     d @@pgm           s             10
     d kkpgm           s             10

     c*-- Key list --*
     c*
     c* Entry key list
     c*
     C     *entry        plist
     C                   parm                    @@pgm
     C*
     C                   call      'WA001R'
     C                   parm                    ##co#             3 0
     C                   parm                    ##fnd             3 0
     C                   parm                    ##conm           40
     C                   parm                    ##fnam           40

     c                   time                    dsdttm
     c     *usa          move      dsdate        sys_date
     c     *cymd         move      sys_date      sys_date7
     C*
      /Free
       exsr MainSr;

       begsr MainSr;
          exsr popStWkFl;
          *inlr = *on;
       endsr;

       begsr popStWkFl;
       read pst02;
          dow not %eof(ststp);
             SSco# = ##co#;
             SSst  = %trim(STkey);
             SSname= %trim(STname);

                kkpgm = @@pgm;
                chain kkpgm pss0t;
                   if %found(wtsselp);
                      SSiefl = SSdief;
                         if SSiefl = *blanks;
                            SSiefl = 'I';
                         endif;
                   else;
                      SSiefl = 'I';
                   endif;
            write pss01;
            read pst02;
          enddo;
       endsr;
