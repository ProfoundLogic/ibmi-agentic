      *********************************************************************
      * REVISIONS:
     *
     *
      *********************************************************************
     D*=====================================================================
     D* Generic popup procedures
     p*=====================================================================
     p* PopDataSaved - Data saved popup window
     p*  Returns - Ok button Response
     P*=====================================================================
     DPopDataSaved     pr              n
      *
     p*=====================================================================
     p* PopSaveNeed - Data save needed popup window
     p*  Returns - Ok button Response
     P*=====================================================================
     DPopSaveNeed      pr              n
      *
     p*=====================================================================
     p* PopOneBtn   - One button popup
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Optional Button text (10A)
     p*  Parm4 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm5 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - button Response
     P*=====================================================================
     DPopOneBtn        pr              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtnTxt                      10a   const options(*nopass)
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     p*=====================================================================
     p* PopupTwoBtn - Popup to show Two buttons
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Button 1 text (10A) optional
     p*  Parm4 - Button 2 text (10A) optional
     p*  Parm3 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm4 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - *On for button 1 and *Off for button 2
     P*=====================================================================
     DPopTwoBtn        pr              n
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtn1Txt                     10a   options(*nopass) const
     D inBtn2Txt                     10a   options(*nopass) const
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
     p*=====================================================================
     p* PopThreeBtn  - Popup to show Three buttons             with message
     p*  Parm1 - Message (256A)
     p*  Parm2 - Panel Title (50A)
     p*  Parm3 - Button 1 text (10A) optional
     p*  Parm4 - Button 2 text (10A) optional
     p*  Parm5 - Button 3 text (10A) optional
     p*  Parm6 - Optional Left (x-coordinate) pixel value (5,0)
     p*  Parm7 - Optional Top (y-coordinate)r pixel value (5,0)
     p*  Returns - *On for button 1, *Off for 2, and blanks for 3
     P*=====================================================================
     DPopThreeBtn      pr             1a
     D inMessage                    256a   const
     D inTitle                       50a   const
     D inBtn1Txt                     10a   options(*nopass) const
     D inBtn2Txt                     10a   options(*nopass) const
     D inBtn3Txt                     10a   options(*nopass) const
     D inLeft                         5  0 options(*nopass) const
     D inTop                          5  0 options(*nopass) const
      *
