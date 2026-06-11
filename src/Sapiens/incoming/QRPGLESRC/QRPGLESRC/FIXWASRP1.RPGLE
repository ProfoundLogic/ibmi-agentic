     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      /title - Fix pgm for SRTODT vs SRRPMY fields
      *=========================================================================
     fwasrl12   uf   e           k disk    rename(psr01:psr12)
     fwdell1    if   e           k disk    rename(pel01:pel01)
     fwtabl     if   e           k disk
      *========================================
     d svFnd           s                   like(SRfnd)
     d svEmp#          s                   like(SREmp#)
     d svDiv           s                   like(SRDiv)
     d svFyr           s                   like(SRFyr)
     d keyTable        s                   like(BSkey)
      /Free
       Read psr12;
       Dow not(%Eof);
          If SRfnd <> svFnd or SRemp# <> svEmp# or
           SRdiv <> svDiv or SRfyr <> svFyr;
             svFnd = SRfnd;
             svEmp# = SRemp#;
             svDiv = SRdiv;
             svFyr = SRfyr;
             Chain (SRco#:SRfnd:SRfyr:SRemp#:SRdiv) pel01;
             If Not(%Found);
                Read psr12;
                Iter;
             Endif;
             keyTable = %Editc(ELbscd:'X');
             Chain (ELco#:keyTable) pbs01;
             If Not(%Found);
                Read psr12;
                Iter;
             Endif;
          Endif;
          If SRrpmy <> 0 and SRtodt <> 0;
             If BSfreq = 'W';
                 SRrpmy = 0;
             Else;
                 SRtodt = 0;
             Endif;
             Update psr12;
          Endif;
       Read psr12;
       Enddo;
       *Inlr = *On;
       Return;
