      // @MBRTEXT: Exercise 2: Hello World RPG Program
      /DEFINE PROFOUNDUI

     H DFTACTGRP(*NO)

     FHELLOD    CF   E             WorkStn Handler('PROFOUNDUI(HANDLER)')

      /FREE

        DoW Not *In03;
          ExFmt HELLOR;
          If Not *In03;
            If NAME <> *Blanks;
              mycolor = 'red';
              HELLO = 'Hello, ' + %Trim(Name) + '.';
            Else;
              HELLO = *Blanks;
            EndIf;
          EndIf;
        EndDo;

        *InLr = *On;
        Return;

      /END-FREE
