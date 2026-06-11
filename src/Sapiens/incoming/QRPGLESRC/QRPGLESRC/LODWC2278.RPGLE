     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('STBNDDIR')
     H dftactgrp(*no)
      //********************************************************************
     H/TITLE LODWC2278 - Load data for division specific flag, Seq #
      //********************************************************************
      //********************************************************************
      // Created: 06/10/21    C:298   Project: WC2278
      //********************************************************************
      // Revisions:

      //********************************************************************
     fwmecl1    uf   e           k disk    rename(pec01:pec01)
     fwma5l     uf   e           k disk    rename(pac01:pac00)
     fwmapl     uf   e           k Disk    rename(pap01:pap00)

     d svCo#           s              3  0
     d @@Co#           s              3  0
     d svFnd           s              3  0
     d @@Fnd           s              3  0
     d svEmp#          s              9  0
     d @@Emp           s              9  0
     d svFein          s              9  0
     d @@Fein          s              9  0
     d svSeq#          s             15  0
     C*
      /Free
       @@co# = 1;
       @@Fnd = 1;
       @@Emp = 1;
       Setll (@@co#:@@Fnd:@@Emp)  pec01;
       Read pec01;
       If not %eof(wmecl1);
          ECseq# = 1;
       Endif;
       Dow not %eof(Wmecl1);

          svCo# = ECco#;
          svFnd = ECfnd;
          svEmp# = ECemp#;
          svSeq# = ECseq#;
          ECdivspec = 'Y';

          Update pec01;
          Unlock wmecl1;

          Read pec01;

          If (svFnd = ECfnd) and (svEmp# = ECemp#)
           and (svCo# = ECco#);
             ECseq# = svSeq# + 1;
          Else;
             ECseq# = 1;
          Endif;
       Enddo;

       @@Fein = 1;
       Setll (@@Co#:@@Fein) pac00;
       Read pac00;
       If not %eof(wma5l);
          A5seq# = 1;
       Endif;
       Dow not %eof(wma5l);

          svCo# = A5co#;
          svFein = A5fein;
          svSeq# = A5seq#;

          Update pac00;
          Unlock wma5l;

          Read pac00;

          If (svCo# = A5co#) and (svFein = A5fein);
             A5seq# = svSeq# + 1;
          Else;
             A5seq# = 1;
          Endif;
       Enddo;

       Setll (@@Co#:@@Fein) pap00;
       Read pap00;
       If not %eof(wmapl);
          APseq# = 1;
       Endif;
       Dow not %eof(wmapl);

          svCo# = APco#;
          svFein = APfein;
          svSeq# = APseq#;

          Update pap00;
          Unlock wmapl;

          Read  pap00;

          If (svCo# = APco#) and (svFein = APfein);
             APseq# = svSeq# + 1;
          Else;
             APseq# = 1;
          Endif;
       Enddo;
       *inlr = *on;
      /End-free
