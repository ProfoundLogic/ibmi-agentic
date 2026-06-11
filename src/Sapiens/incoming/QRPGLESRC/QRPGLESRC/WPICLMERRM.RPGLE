     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/TITLE WPICLMERR - Profound Claim Error Processing Modules
     h/DEFINE   PROFOUNDUI
     h nomain
     h DFTNAME(WPICLMERRM)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Has Edit Procedure Modules.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 7/25/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*  06/03/15 C:291 Phaseware: 33828
     h*                 Fix date error when 6 digit dates entered
     h*  07/30/13 C:180 CI114  Add procedure to check for invalid
     h*                        characters.
     h*
     h**********************************************************************
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fsmupl1    if   e           k Disk

     d* Proto-types


     d* General Prototypes
     d/copy wPrClmErrM
     d/copy sPrDate

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
      *
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // ChkNotBlank - Blank Not Allowed
       //===================================================================
     P ChkNotBlank     B                   Export
     D ChkNotBlank     PI              n
     D valueIn                       80
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If valueIn=*Blanks;
          errorMsgOut='This field can not be blank.';
          errorMsgIdOut='WCR0503';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkNotBlank     E
       //===================================================================
       // ChkNotZero - Zero Not Allowed
       //===================================================================
     P ChkNotZero      B                   Export
     D ChkNotZero      PI              n
     D valueIn                       15  2
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If valueIn=*Zeros;
          errorMsgOut='This field can not be less than, or equal to zero.';
          errorMsgIdOut='WCR0502';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkNotZero      E
       //===================================================================
       // ChkBlank - Blank Is Required
       //===================================================================
     P ChkBlank        B                   Export
     D ChkBlank        PI              n
     D valueIn                       80
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If valueIn<>*Blanks;
          errorMsgOut='This field must be blank.';
          errorMsgIdOut='WCR0450';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkBlank        E
       //===================================================================
       // ChkZero - Zero is Required
       //===================================================================
     P ChkZero         B                   Export
     D ChkZero         PI              n
     D valueIn                       15  2
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If valueIn<>*Zeros;
          errorMsgOut='This field must be zero.';
          errorMsgIdOut='       ';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkZero         E
       //===================================================================
       // ChkDate - Check to See If Date Is Valid
       //===================================================================
     P ChkDate         B                   Export
     D ChkDate         PI              n
     D valueIn                        8  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10
     D*** Begin del *** 06/03/15 *******************************************
     D*** Begin Add *** 11/03/14 *******************************************
     D***              ds
     D**date                   1      8  0
     D**year                   5      8  0
     D*** End   Add *** 11/03/14 *******************************************
     D*** End del ***** 06/03/15 *******************************************

      /Free

       //*** Begin del *** 06/03/15 *******************************************
       //*** Begin Add *** 11/03/14 *******************************************
       //date=valueIn;
       //If year>=1900;
       //*** Begin Add *** 11/03/14 *******************************************
       //*** End   del *** 06/03/15 *******************************************
          If dtCheck(valueIn)=0;
             errorMsgOut='Invalid date entered.';
             errorMsgIdOut='WCR0500';
             Return *On;
          //*** Begin Add *** 06/03/15 *******************************************
          Else;
            If DTcymd(valueIn)<0;
              errorMsgOut='Invalid date entered.';
              errorMsgIdOut='WCR0500';
              Return *On;
            Else;
              Return *Off;
            Endif;
          Endif;
       //*** End   Add *** 06/03/15 *******************************************
       //*** Begin Del *** 06/03/15 *******************************************
       // Else;
       //    Return *Off;
       // EndIf;
       //*** Begin Add *** 11/03/14 *******************************************
       //Else;
       // errorMsgOut='Invalid date entered.';
       // errorMsgIdOut='WCR0500';
       // Return *On;
       //EndIf;
       //*** End   Add *** 11/03/14 *******************************************
       //*** End Del ***** 06/03/15 *******************************************

      /End-Free
     P ChkDate         E
       //===================================================================
       // ChkAscDate - Check to See If Dates Ascend
       //===================================================================
     P ChkAscDate      B                   Export
     D ChkAscDate      PI              n
     D valueIn1                       8  0
     D valueIn2                       8  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If dtCYMD(valueIn2) < dtCYMD(valueIn1);
          errorMsgOut='Dates must be in ascending order.';
          errorMsgIdOut='WCR0529';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkAscDate      E
       //===================================================================
       // ChkLtAccDate - Date can't be less than accident date
       //===================================================================
     P ChkLtAccDate    B                   Export
     D ChkLtAccDate    PI              n
     D valueIn                        8  0
     D accidentDate                   7  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If dtCYMD(valueIn) < accidentDate And valueIn<>0;
          errorMsgOut='Date may not be less than accident date.';
          errorMsgIdOut='WCR0572';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkLtAccDate    E
       //===================================================================
       // ChkGtAccDate - Check to See If Date Is Less than other date
       //===================================================================
     P ChkGtAccDate    B                   Export
     D ChkGtAccDate    PI              n
     D valueIn                        8  0
     D compDtIn                       7  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If dtCYMD(valueIn) > compDtIn;
          errorMsgOut='Date can''t be greater than accident date.';
          errorMsgIdOut='WCR0500';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkGtAccDate    E
       //===================================================================
       // ChkFutrDate - Check to see if a future date
       //===================================================================
     P ChkFutrDate     B                   Export
     D ChkFutrDate     PI              n
     D valueIn                        8  0
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       If dtCYMD(valueIn) > DateTo7(systemDate());
          errorMsgOut='Date may not be a future date.';
          errorMsgIdOut='       ';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ChkFutrDate     E
       //===================================================================
       // invalidStaffCd - Checks to see if staff code is invalid
       //===================================================================
     P InvalidStaffCd  B                   Export
     D InvalidStaffCd  PI              n
     D staffCdIn                      3
     D errorMsgOut                   50
     D errorMsgIdOut                 10

      /Free

       Chain staffCdIn pup01;
       If not %Found(SMUPL1) or (%Found(SMUPL1) And UPstat='I');
          errorMsgOut='Invalid Staff Code.';
          errorMsgIdOut='WCR0503';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P InvalidStaffCd  E
       //===================================================================
       // ChkBadChar  - Bad characters not allowed              07/30/13
       //===================================================================
     P ChkBadChar      B                   Export
     D ChkBadChar      PI              n
     D valueIn                       80
     D errorMsgOut                   50
     D errorMsgIdOut                 10
     D IX              S              5  0

      /Free

         For IX = 1 to 80;
             If %subst(valueIn:IX:1) < X'40' or
                %subst(valueIn:IX:1)= X'41' or
                %subst(valueIn:IX:1) > X'F9';
                errorMsgOut='Invalid character found.';
                errorMsgIdOut='W400073';
                Return *On;
                leave;
             EndIf;
         endfor;

         If ix = 81;
            Return *OFF;
         Endif;
      /End-Free
     P ChkBadChar      E
