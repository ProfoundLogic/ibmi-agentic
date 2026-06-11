     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
      *=========================================================
      * Comments
      *=========================================================
      * You can't call this pgm directly. You must call FIX32485C.
      * The update flag controls whether production files are
      *  DELETED or not.
      * Only the Effective Date and Co# are required. The
      *  other parms can come in as blanks.
      * The program calls W0050R1 to delete all files for old
      *  renewal quotes.
      * The Effective Date is used to delete quote renewals
      *  with an effective date less than the value of this
      *  entry parm.
      *=========================================================
      * Files
      *=========================================================
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwmeml     uf   e           k disk    rename(pem01:pem00)
     fwdelp     o    e             disk    rename(pel01:pel0p)
      *=========================================================
      * Procedures
      *=========================================================
     d*entry
     d Fix32485        pr
     d  @@update                      1
     d  @@efdt                        7
     d  @@co#                         3
     d  @@fnd                         3
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d Fix32485        pi
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
     d DeletePolicy    pr                  extpgm('W0050R01')
     d  qqCo#                              like(ELco#)
     d  qqFnd                              like(ELfnd)
     d  qqEmp#                             like(ELemp#)
     d  qqDiv                              like(ELdiv)
     d  qqQteFlg                      1    const
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
       ChkStack('FIX32485C':qqFound);
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
       Setll (xxCo#) pel00;
       Reade (xxCo#) pel00;
       Dow not(%eof);
        // Skip records that don't meet filters
          If xxFnd <> 0 and xxFnd <> ELfnd;
             Reade (xxCo#) pel00;
             Iter;
          Endif;
          If xxEmp# <> 0 and xxEmp# <> ELemp#;
             Reade (xxCo#) pel00;
             Iter;
          Endif;
          If xxDiv <> *blank and ELdiv <> %int(xxDiv);
             Reade (xxCo#) pel00;
             Iter;
          Endif;
          If xxFyr <> 0 and xxFyr < ELfyr;
             Reade (xxCo#) pel00;
             Iter;
          Endif;
          If xxEfDt <= ELpfdt;
             Reade (xxCo#) pel00;
             Iter;
          Endif;
          If ELrqfl <> 'Y';
             Reade (xxCo#) pel00;
             Iter;
          Endif;
        // Write to archive file
          Write pel0p;
        // Delete policy if in update mode
          If @@update = 'Y';
             Callp DeletePolicy (ELco#:ELfnd:ELemp#:ELdiv:'Q');
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem00;
             If %found;
                Delete pem00;
             Endif;
          Endif;
       Reade (xxCo#) pel00;
       Enddo;
       *Inlr = *on;
       Return;
      /end-free
