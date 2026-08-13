      // @MBRTEXT: Product Description vi AI
          ctl-opt option(*nodebugio: *srcstmt);

          dcl-f proddesc1d workstn ;

          clear scrn1;

          dou *in03;

             exfmt scrn1;

          enddo;

          *inlr = *on;
          return;
