     D*=====================================================================
     D* Notifications related procedure prototypes
     D*=====================================================================
     D*                                                                    *
     D* Revisions:                                                         *
     D*                                                                    *
     D* 08/04/14 C:65  Project:  WC2277                                    *
     D*                Add new procedure to display an outlook window      *
     D*                for editing email information.                      *
     D*                                                                    *
     D*                                                                    *
     D*=====================================================================

     P*=====================================================================
     P* GenerateNotice - Write/Updates records to the notification master/detail files.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*          Parm 4 = Key  (type of document)   (20 A)
     P*          Parm 5 = Document ID               (12 A)
     P*          Parm 6 = Folder                    (80 A)
     P*          Parm 7 = Seq#                      (5.0S)
     P*          Parm 8 = UNCN                     (255 A)  {Optional}
     P*=====================================================================
     DGenerateNotice   pr
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D inKey                         20    const
     D inDcid                        12    const
     D inFold                        80    const
     D inSeq#                         5  0 const
     D inUNCN                       255    options(*nopass) const
     D*
     P*=====================================================================
     P* SendNotice - Generates record to data queue for sending email notification.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*=====================================================================
     DSendNotice       pr             1
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
     D*
     P*=====================================================================
     P* PolicyLevCheck - Checks if the notification set is policy level.
     P*          Parm 1 = Type                      ( 1 A)
     P*=====================================================================
     DPolicyLevCheck   pr             1
     D inType                        20    const
     D*
     P*=====================================================================
     P* EmailUpdate - Display outlook syle popup window for editing email info.
     P*          Parm 1 = Relative Record #         (9.0S)
     P*          Parm 2 = Mode (E = WDELP, C=WMCMP) ( 1 A)
     P*          Parm 3 = Type (type of process)    (20 A)
     P*=====================================================================
     DEmailUpdate      pr             1
     D inRRN                          9  0 const
     D inMode                         1    const
     D inType                        20    const
