      // @MBRTEXT: Page at a time subfile
       // Control Specifications
       Ctl-Opt DftActGrp(*No);

       // File Specifications
       Dcl-F SFL3D WorkStn(*Ext) Handler('PROFOUNDUI(HANDLER)')
       SFile(SFL1 : SFLRRN);
       Dcl-F PRODP Disk(*Ext) Usage(*Update) Keyed;

       // Variables
       Dcl-S SFLRRN Int(5);
       Dcl-S PRODRRN Int(5);
       Dcl-S i Int(5);
       Dcl-S SETRRN Int(5);
       Dcl-S KeyProd Packed(7:0);

       // Constants
       Dcl-C SFLPAG Const(13);



       // Mainline Processing
       DoU EXIT = *On;
          // Clear the subfile.
          SFLCLR = *On;
          Write SFLCTL1;
          SFLCLR = *Off;
         // Reads PageUp PageDown response indicators and sets PRODP file cursor.
          ExSr PositionFile;
          // Load one page of records.
          SFLRRN = 0;
          Read PROD;
          DoW (Not %EOF(PRODP)) and SFLRRN < SFLPAG;
             SFLRRN += 1;
             PRODRRN += 1;
             Write SFL1;
             Read PROD;
          EndDo;
          If %EOF(PRODP);
             EnableDown = *Off;
          Else;
             EnableDown = *On;
          EndIf;
          If PRODRRN <= SFLPAG;
             EnableUp = *Off;
          Else;
             EnableUp = *On;
          EndIf;
          // Display the subfile.
          SFLDSP = *On;
          ExFmt SFLCTL1;
       EndDo;
       *InLr = *On;
       Return;



       ////////////////////////////////////////////////////////////////////////
       // //
       // PositionFile //
       // //
       ////////////////////////////////////////////////////////////////////////
       BegSr PositionFile;
          Select;
             When PRODRRN = 0; // First load.
                SETRRN = 1;
             When PageDown = *On; // Page down.
                SETRRN = PRODRRN + 1;
             When PageUp = *On; // Page up.
                PRODRRN -= SFLRRN;
                PRODRRN -= SFLPAG;
                SETRRN = PRODRRN + 1;
             Other; // Enter or other key pressed. Do nothing.
                LeaveSr;
          EndSL;
          KeyProd = SETRRN;
          SetLL KeyProd PROD;
       EndSr;