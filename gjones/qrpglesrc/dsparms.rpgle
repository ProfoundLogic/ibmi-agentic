      // @MBRTEXT: Receives parms as a datastructure
         dcl-c true '1';

         dcl-ds inputdsDef qualified;
            dcl-s char1 char(10);
            dcl-s num1 packed(5:0);
            dcl-s char2 char(30);
         end-ds;

         dcl-pi *n;
            parmsIn likeds(inpudsDef);
         end-pi;


         dump(a);

         *inlr = true;
