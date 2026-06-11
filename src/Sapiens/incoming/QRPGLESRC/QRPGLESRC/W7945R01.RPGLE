     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*CALLER)
     H/TITLE W7945R01
      //----------------------------------------------------------------|
      //  Description: Populates WXCKP workfile for EFT/ACH processing  |
      //                                                                |
      //  Called from: W7945C                                           |
      //                                                                |
      //  Calls to: WA001R                                              |
      //                                                                |
      //  Created: C:191  Project: WC1806  Dated: 06/29/09              |
      //----------------------------------------------------------------|
      //  Revisions:                                                    |
      //                                                                |
      //  10/21/15 C:136 Project: WC2337                                |
      //                 Do not write bank select record if no          |
      //                 EFT transactions exist for it                  |
      //                                                                |
      //  05/28/14 C:136 Project: 129WTG                                |
      //                 Populate new fields to group EFT transactions  |
      //                                                                |
      //   05/05/14  C:302 SUPPORT WA146 85535                          |
      //                   If existing lock is for same user, ignore.   |
      //________________________________________________________________|

     FW7945F01  cf   e             workstn usropn
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      //*  Physical file definition (for refreshing subfile record)
     Fwxckp     uf a e             DISK
     Fwsckl     if   e           k disk    rename(pck01:pcs01)
     F                                     prefix(cs:2)
     Fwaefl     if   e           k disk    rename(pef01:pef00)
     Fsmbup     if   e           k disk

     D xxUser          s             10a   inz
     D ##Co#           s              3  0
     D ##Fnd           s              3  0
     D ##Conm          s             40a
     D ##Fnam          s             40a

     D psds          esds                  extname(szq1p)

     D Const1          C                   'EFT batch job '
     D Const2          C                   ' previously submitted by '
     D Const3          C                   ' has not completed.'
     D Const4          C                   'Process currently in use by '

     D W7945R01        PR
     D                                1
     D                                1

     D W7945R01        PI
     D  @@rtn                         1
     D  @@mode                        1

     D GetCoAndFnd     PR                  Extpgm('WA001R')
     D  ##Co#                         3  0 Const
     D  ##Fnd                         3  0 const
     D  ##Conm                       40    const
     D  ##Fnam                       40    const

     D*--------------------------------------------------
     D* Procedure name: isLocked
     D* Purpose:        Check to see if someone else has the process locked.
     D* Returns:        True if it is locked
     D*--------------------------------------------------
     D isLocked        PR              N

     D*--------------------------------------------------
     D* Procedure name: Process
     D* Purpose:        Process the bank records
     D* Returns:
     D*--------------------------------------------------
     D Process         PR

     D*--------------------------------------------------
     D* Procedure name: isEligible
     D* Purpose:        Process the bank records
     D* Returns:        boolean
     D*--------------------------------------------------
     D isEligible      PR              n

      /Free

       *inlr = *On;
       @@rtn = 'Y';
       xxUser = Q1User;

       If not isLocked();
          Process();
       Endif;

      /End-free

     P*---------------------------------------------------------------------
     P* Procedure name: CheckForLocks
     P* Purpose:        Check to see if someone else has the process locked.
     P* Returns:        True if it is locked.
     P*---------------------------------------------------------------------
     P isLocked        B
     D isLocked        PI              N
      /Free

       // Read WXCKP to see if there are any records in it (only if mode <> D)
       If @@mode <> 'D';
          Read pck01;
          // If so, display a hard edit preventing further processing.
          If not %EOF;
             //  If it is the same user locking the records, reset the file.
             If xxUser = CKuser;
                Dow not %EOF;
                   Delete wxckp;
                   Read pck01;
                Enddo;
                Return *Off;
             Else;
                Open w7945f01;
                If CKjob <> *Blanks;
                   scMsg = Const1 + %trim(CKjob) + Const2
                    + %trim(CKuser) + Const3;
                Else;
                   scmsg = Const4 + %trim(CKuser) + '.';
                Endif;

                Exfmt wdw01;
                @@rtn = 'N';
                Close w7945f01;
                Return *On;
             Endif;

          Endif;

       Endif;
       Return *Off;

      /End-free
     P isLocked        E

     P*--------------------------------------------------
     P* Procedure name: Process
     P* Purpose:        Process the bank records
     P* Returns:
     P*--------------------------------------------------
     P Process         B
     D Process         PI
      /Free

       // Retrieve Company and Fund Names
       GetCoAndFnd(##co#:##fnd:##conm:##fnam);
       // Get EFT flags from Monthly Batch Backup
       Read SMBUP;
       // loop through WSCKP and retrieve those bank accounts in which EFT information has been set
       Setll (##co#) pcs01;
       Reade (##co#) pcs01;
       Dow not %EOF;
        // Check EFT setup
          If CSeftf <> *Blanks and CSidst <> *Zeros
           and CSiorg <> *Zeros and CSordi <> *Zeros
           and CSsvcl <> *Zeros;
        // Skip bank accounts that do not have an EFT transaction
        //  that is flagged to create EFT transmission in daily batch
             Setll (CSco#:CSacct) pef00;
             Reade (CSco#:CSacct) pef00;
             Dow not(%Eof);
                If %Eof;
                   Leave;
                Endif;
                If isEligible();
                   CKco# = CSco#;
                   CKacct = CSacct;
                   CKinex = 'I';
                   CKuser = xxUser;
                   CKjob = *Blanks;
                   CKidst = %editc(CSidst:'X');
                   CKeftf = CSeftf;
                   CKbknm = CSai01;
                   CKbklt = CSbklt;
                   CKsvcl = CSsvcl;
                   Write pck01;
                   Leave;
                Endif;
             Reade (CSco#:CSacct) pef00;
             Enddo;
          Endif;
          Reade (##co#) pcs01;
       Enddo;
      /End-free
     P Process         E
      *==============================================================
     P isEligible      B
     D isEligible      PI              n
      /free
       If @@mode <> 'D';
          Return *on;
       Endif;
       If BUceft = 'Y' and (EFtype = 'C' or EFtype = 'D' or EFtype = 'P')
        or BUaeft = 'Y' and EFtype = 'A'
        or BUeeft = 'Y' and EFtype = 'E';
          Return *on;
       Else;
          Return *off;
       Endif;
      /end-free
     P isEligible      E
