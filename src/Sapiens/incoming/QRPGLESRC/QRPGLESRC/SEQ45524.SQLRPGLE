      /Free
       Exec SQL
        set option commit = *none;
       Exec SQL
        update WMALP
         set ALELTP = 'L'
         where ALELTP = 'Y';
       *inlr = *on;
      /End-free
