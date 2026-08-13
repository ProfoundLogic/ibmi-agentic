      // @MBRTEXT: Load Subfile Example
       // Control Specifications
       Ctl-Opt DftActGrp(*No);

       // File Specifications
       Dcl-F SFL1D WorkStn(*Ext) Handler('PROFOUNDUI(HANDLER)')
       SFile(SFL1 : RRN);
       Dcl-F PRODP Disk(*Ext) Usage(*Input) Keyed;


       // Variables
       Dcl-S RRN Int(5);

       // Mainline Processing
       // Initialize "named indicators.
       REFRESH = *Off;
       EXIT = *Off;
       SFLDSP = *Off;
       SFLCLR = *Off;

       DoW EXIT = *Off;
          // Clear the subfile.
          SFLCLR = *On;
          Write SFLCTL1;
          SFLCLR = *Off;
          RRN = 0;

          // Load the subfile.
          SetLL *LoVal PROD;
          Read PROD;
          DoW Not %EOF(PRODP);
             RRN += 1;
             Write SFL1;
             Read PROD;
          EndDo;

          // Display the subfile.
          SFLDSP = *On;
          ExFmt SFLCTL1;
       EndDo;
       *InLr = *On;
       Return;