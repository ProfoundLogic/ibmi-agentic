     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
      *=========================================================
      * Comments
      *=========================================================
      * You can't call this pgm directly. You must call REVRETROC.
      * The update flag controls whether production files are
      *  updated or not.
      * The effective date is used as a key to WDEHL7 to target
      *  policies to process.
      * Only the Effective Date and Co# are required. The
      *  other parms can come in as blanks.
      * The program deletes the selected retro premium history
      *  record if flagged to do so.
      * It subtracts the Change Retro Premium Amount (EHXRPR) from
      *  the running Retro total in WDELP (ELstam), clears the Final
      *  Settlement flag, and reverts the Retro Settlement Date
      *  to its previous value.
      *=========================================================
      * Files
      *=========================================================
     fwdehl7    uf   e           k disk    rename(peh01:peh07)
     fwdehl14   if   e           k disk    rename(peh01:peh14) prefix(l14)
     fwdell     uf   e           k disk    rename(pel01:pel00)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwdelp     o    e             disk    rename(pel01:pel0p)
     fwdehp     o    e             disk    rename(peh01:peh0p)
      *=========================================================
      * Procedures
      *=========================================================
     d*entry
     d RevRetro        pr
     d  @@update                      1
     d  @@efdt                        7
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d RevRetro        pi
     d  @@update                      1
     d  @@efdt                        7
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1
     d GetLastRun      pr             7  0
      *=========================================================
      * Pgm Fields
      *=========================================================
     d xxEfDt          s              7  0
     d xxCo#           s              3  0
     d xxFnd           s              3  0
     d xxEmp#          s              9  0
     d xxDiv           s              5
     d xxFyr           s              3  0
     d qqFound         s              1
      *=========================================================
      * Main
      *=========================================================
      /free
       ChkStack('REVRETROC':qqFound);
       If qqFound <> '1';
          *inlr = *on;
          Return;
       Endif;
       xxEfDt = %int(@@efdt);
       xxCo#  = %int(@@co#);
       If @@fnd <> *blank;
          xxFnd  = %int(@@fnd);
       Endif;
       If @@emp# <> *blank;
          xxEmp# = %int(@@emp#);
       Endif;
       xxDiv  = @@div;
       If @@fyr <> *blank;
          xxFyr  = %int(@@fyr);
       Endif;
       Setll (xxCo#) pfn00;
       Reade (xxCo#) pfn00;
       Dow not(%eof);
        // Skip records that don't meet the filter
          If xxFnd <> 0 and xxFnd <> FNfnd;
             Read pfn00;
             Iter;
          Endif;
          Setgt  (FNco#:FNfnd:xxEfDt) peh07;
          Readpe (FNco#:FNfnd:xxEfDt) peh07;
          Dow not(%eof);
           // Skip records that don't meet the filters
             If xxEmp# <> 0 and xxEmp# <> EHemp#;
                Readpe (fnco#:fnfnd:xxEfDt) peh07;
                Iter;
             Endif;
             If xxDiv <> *blank and EHdiv <> %int(xxDiv);
                Readpe (fnco#:fnfnd:xxEfDt) peh07;
                Iter;
             Endif;
             If xxFyr <> 0 and xxFyr < EHfyr;
                Readpe (fnco#:fnfnd:xxEfDt) peh07;
                Iter;
             Endif;
           // Process retro records that have a change Retro amount
           // Write out to archive files to the archive library in REVRETROC
           // Update production files if flagged to do so
             If EHfaf = 'X';
                Write peh0p;
                If @@update = 'Y';
                   Delete peh07;
                Endif;
                Chain (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pel00;
                If %found;
                   Write pel0p;
                   If @@update = 'Y';
                      ELfset = *blank;
                      ELstdt = getLastRun();
                      If Elstdt = 0;
                         ELstam = 0;
                      Else;
                         ELstam = ELstam - EHxrpr;
                      Endif;
                      Update pel00;
                   Endif;
                Endif;
             Endif;
          Readpe (FNco#:FNfnd:xxEfDt) peh07;
          Enddo;
       Reade (xxCo#) pfn00;
       Enddo;
       *Inlr = *On;
       Return;
      /end-free
      *===========================================================
      * GetLastRun - returns date of last retro run or zero if none
      *===========================================================
     p GetLastRun      b
     d GetLastRun      pi             7  0
      /free
       Setgt  (EHco#:EHfnd:EHemp#:EHdiv:EHfyr:xxEfDt) peh14;
       Readpe (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh14;
       Dow not(%eof);
          If L14EHfaf = 'X';
             Return l14EHtrdt;
          Endif;
       Readpe (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh14;
       Enddo;
       Return 0;
      /end-free
     p GetLastRun      e
