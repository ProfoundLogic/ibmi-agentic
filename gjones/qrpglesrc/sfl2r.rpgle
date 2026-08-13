      // @MBRTEXT: Input capable subfile example
       // Control Specifications
       Ctl-Opt DftActGrp(*No);

       // File Specifications
       Dcl-F SFL2D WorkStn(*Ext) Handler('PROFOUNDUI(HANDLER)')
       SFile(SFL2 : RRN);
       Dcl-F PRODP Disk(*Ext) Usage(*Update) Keyed;

       // Variables
       Dcl-S RRN Int(5);

       // Mainline Processing
       // Initialize "named indicators.
       UPDATESFL = *Off;
       EXIT = *Off;
       SFLDSP = *Off;
       SFLCLR = *Off;

       DoW EXIT = *Off;
          // Clear the subfile.
          SFLCLR = *On;
          Write SFLCTL2;
          SFLCLR = *Off;
          RRN = 0;
          // Load the subfile.
          SetLL *LoVal PROD;
          Read(N) PROD;
          DoW Not %EOF(PRODP);
             RRN += 1;
             Write SFL2;
             Read(N) PROD;
          EndDo;
          // Display the subfile.
          SFLDSP = *On;
          ExFmt SFLCTL2;
          If UPDATESFL = *On;
             // Read changed records from the subfile.
             ReadC SFL2;
             DoW Not %EOF(SFL2D);
                // Update changed subfile records to database.
                Chain (PRID) PROD;
                Chain RRN SFL2;
                If %Found(PRODP) And %Found(SFL2D);
                   Update PROD;
                EndIf;
                ReadC SFL2;
             EndDo;
          EndIf;
       EndDo;
       *InLr = *On;
       Return;