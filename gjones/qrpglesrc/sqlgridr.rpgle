      // @MBRTEXT: sql grid sample
          /If DEFINED(*CRTBNDRPG)
          // Control Specifications
          Ctl-Opt DftActGrp(*No);
       /EndIf
       // File Specifications
       Dcl-F SQLGRIDD WorkStn(*Ext) Handler('PROFOUNDUI(HANDLER)')
       SFile(SFL : RRN);
       // Variables
       Dcl-S RRN Int(10);
       // Mainline Processing
       EXIT = *Off;
       DoW EXIT = *Off;
          ExFmt SFLCTL;
       EndDo;
       *InLr = *On;
       Return;