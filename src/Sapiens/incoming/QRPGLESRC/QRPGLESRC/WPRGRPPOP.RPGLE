      *********************************************************************
      * Revisions:
      *
      * 01/17/22 C:291  Project: WC2788
      *                 Add GroupCopySetup procedure prototype.
      *
      *********************************************************************
     d ptrGrpDtl       s               *   inz
     d scCo#           s              3s 0 inz
     d scFnd           s              3s 0 inz

      /If defined (WPIGRPPOP)
     d dsGrpWdw      e ds                  extname(wpigrppop:rcdGrpWdw)
     d                                     based(ptrGrpDtl)

      /else
     d dsGrpWdw      e ds                  extname(wpiGrpPop:rcdGrpWdw)  inz

      /Endif

      *=====================================================================
      * Group master procedures
      *=====================================================================
      * LoadDetail - Load Group Detail
      *=====================================================================
     d LoadDetail      pr              n
     d inRRN                          9  0
     d inPtrGrpDtl                     *
     d inMode                         1a   const
      *=====================================================================
      * ClearDetail - Clear Detail Box
      *  Parm1   - Pointer to format DS
      *=====================================================================
     d ClearDetail     pr              n
     d inPtrGrpDtl                     *
      *=====================================================================
      * ValidGrpDtl - Validate Group Detail data
      *  Parm1   - Mode
      *  Parm2   - Pointer to format DS
      *  Parm3   - Group RRN
      *  Returns - Error flag '0' - error, '1' - no error
      *=====================================================================
     dValidGrpDtl      pr              n
     d inMode                         1    const
     d inPtrGrpDtl                     *
     d inFNRrn                        9  0
      *=====================================================================
      * UpdateGrpDtl - Update Group Detail changes to file
      *  Parm1   - Pointer to format DS
      *  Parm2   - RRN of Group Master record
      *  Parm3   - Mode
      *  Returns - Error indicator
      *=====================================================================
     d UpdateGrpDtl    pr              n
     d inPtrGrpDtl                     *
     d inRRN                          9  0
     d inMode                         1
      *=====================================================================
      * DspGrpDtl - Display Group Detail Screen
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'C'-Change, 'D'-Display,
      *  Returns - success indicator
      *=====================================================================
     d DspGrpDtl       pr              n
     d inFNrrn                        9  0
     d inMode                         1
      *** Beg Add ***** 01/17/22 ******************************************
      *=====================================================================
      * GroupCopySetup - Copy Group configuration setup
      *=====================================================================
     dGroupCopySetup   pr
     d inRRN                          9  0 const
     d inData                       500a   const
     d inPtrEndFrm                     *   const Options(*nopass)
      *** End Add ***** 01/17/22 ******************************************
      *=====================================================================
      * AddRegInfo - Add Region Information
      *=====================================================================
     d AddRegInfo      pr
     d inRRN                          9  0
     d inRRN1                         9  0
     d inEdit                         1a   const
     d inMode                         1a   const
      *=====================================================================
      * AddChpInfo - Add Chapter Information
      *=====================================================================
     d AddChpInfo      pr
     d inRRN                          9  0
     d inRRN1                         9  0
     d inEdit                         1a   const
     d inMode                         1a   const
      *=====================================================================
      * AddBnkInfo - Add Bank Information
      *=====================================================================
     d AddBnkInfo      pr
     d inRRN                          9  0
     d inRRN1                         9  0
     d inEdit                         1a   const
     d inMode                         1a   const
     d inRepeat                        n
      *=====================================================================
      * procAddGrp - Process Group Addition
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'C'-Change, 'D'-Display,
      *  Returns - success indicator
      *=====================================================================
     d procAddGrp      pr              n
     d inFNrrn                        9  0
     d inMode                         1
      *=====================================================================
      * procAddBnk - Process Bank Addtion
      *  Parm1   - Group RRN
      *  Parm2   - Mode(1A) 'C'-Change, 'D'-Display,
      *  Returns - success indicator
      *=====================================================================
     d procAddBnk      pr              n
     d inFNrrn                        9  0
     d inMode                         1
      *=====================================================================
      * AddPolYear - Add/Edit Policy year
      *  Parm1   - RRN
      *  Parm2   - Mode(1A) 'C'-Change, 'D'-Display,
      *  Returns - success indicator
      *=====================================================================
     d AddPolYear      pr              n
     d inFNrrn                        9  0
     d inMode                         1
