      // @MBRTEXT: File Maintenance Example
       // Control Specifications
       Ctl-Opt DftActGrp(*No) Option(*SrcStmt:*NoDebugIO);

       // File Specifications
       Dcl-F MAINTD WorkStn(*Ext) Handler('PROFOUNDUI(HANDLER)')
       SFile(MAINTSFL : RRN);
       Dcl-F PRODP Disk(*Ext) Usage(*Update) Keyed;
       Dcl-F CATEGP Disk(*Ext) Usage(*Input) Keyed;

       // Variables
       Dcl-S RRN Int(10);
       Dcl-S Valid IND;


       // Mainline Processing
       SFLDSP = *Off;
       SFLCLR = *Off;
       VIEW = *Off;
       CHANGE = *Off;
       UPDATEREC = *Off;
       EXIT = *Off;
       CANCEL = *Off;

       DoW EXIT = *Off;
          // Reset icon responses.
          VIEW = *Off;
          CHANGE = *Off;
          // Clear subfile.
          SFLCLR = *On;
          Write MAINTCTL;
          SFLCLR = *Off;
          RRN = 0;

          // Load subfile.
          SetLL *LoVal PROD;
          Read(N) PROD;
          DoW Not %EOF(PRODP);
             RRN += 1;
             Chain (PCAT1) CATEG;
             If Not %Found();
                CNAME = *Blanks;
             EndIf;
             Write MAINTSFL;
             Read(N) PROD;
          EndDo;
          // Display subfile.
          SFLDSP = *On;
          ExFmt MAINTCTL;
          SFLDSP = *Off;
          // Check for icon click.
          ReadC MAINTSFL;
          If Not %EOF();
             // Lock record for update, if in change mode.
             If CHANGE = *On;
                Chain (PRID) PROD;
       //      else;                          // added to fix the detail screen bug
       //         chain(n) (PRID) PROD;       // added to fix the detail screen bug
             EndIf;
             // Display detail screen.
             Valid = *Off;
             DoW Not Valid;
                ExFmt MAINTDTL;
                // Perform field validation if the record is being updated.
                Valid = *On;
                CATERROR = *Off;
                If UPDATEREC = *On;
                   Chain (PCAT1) CATEG;
                   If Not %Found();
                      CATERROR = *On;
                      Valid = *Off;
                   EndIf;
                EndIf;
             EndDo;
             // Update record if necessary.
             If UPDATEREC = *On;
                Update PROD;
             ElseIf CHANGE = *On;
                // Otherwise unlock the record if in change mode.
                Unlock PRODP;
             EndIf;

          EndIf;
       EndDo;
       *InLr = *On;
       Return;