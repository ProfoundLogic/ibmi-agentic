     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H*****************************************************************
     H/TITLE FA777R - API for restoring bill to original state
     H* *****************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*  This API removes all lines from message/eob files added by     *
     H*  new line from claimcheck                                       *
     H*******************************************************************
     H*                                                                 *
     H*  Called From:                                                   *
     H*    FA735R: Bill Review and Repricing API                        *
     H*                                                                 *
     H*  Calls To: None                                                 *
     H*                                                                 *
     H*  Created:  06/11/12  C:251   Project: 114MER                    *
     H*                                                                 *
     H*******************************************************************
     H*  REVISIONS:                                                     *
     H*                                                                 *
     H*  10/03/13 C:079 SUPPORT: MC147 83581                            *
     H*                 Do not delete user entered EOB Codes/Notes      *
     H*                 Also on new lines delete eob codes/messages     *
     H*                                                                 *
     H*  10/09/12 C:079 PROJECT: 114MER                                 *
     H*                 Change how original charges are restored        *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
     Ffaeop     if   e             disk
     Ffddal4    uf   e           k disk
     Ffdmgl1    uf   e           k disk
     Ffddel     uf   e           k disk
     Ffdcal13   uf   e           k disk
     Ffddhl     uf   e           k disk
     Ffddul     uf   e           k disk
     Ffdkhl2    uf   e           k disk
     F*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D#1rrn            s              9  0
     Dkkco#            s                   like(caco#)
     Dkksub#           s                   like(casub#)
     Dkkfnd            s                   like(cafnd)
     Dkkfyr            s                   like(cafyr)
     Dkkcase           s                   like(cacase)
     Dkkeob#           s                   like(caeob#)
     Dkkrcn            s                   like(carcn)
     Dkkseq#           s                   like(deseq#) inz(*hival)
     C*
     C*  KEY LISTS
     C*
     C     deky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     deky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
    C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
    C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
    C*
     C     mgky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
     C                   kfld                    kkseq#
    C*
     C     mgky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
     C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C*
     C     khky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    cadseq
     C*
     C     daky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkrcn
     C                   kfld                    cadseq
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9            FAEOP
     C*
     C*****************************************************************
     C**                      MAIN PROGRAM                           **
     C*****************************************************************
      /free
       #1rrn = %int(@@rrn);
       chain #1rrn peo01;
       if %found;

        //initialize fields
       exsr setup;

        //Check individual FDCAP lines
       exsr chkLine;
       endif;

       *inlr = *on;
       return;

       //****************************************************************
       // SUBROUTINE  ****** S E T U P **********************************
       //****************************************************************
       Begsr Setup;
        // Set key fields
       kkco# = eoco#;
       kksub# = eosub#;
       kkfnd = eofnd;
       kkfyr = eofyr;
       kkcase = eocase;
       kkeob# = eoeob#;
       kkrcn = eorcn;
       Endsr;

       //****************************************************************
       // Check each line of FDCAP
       //****************************************************************
       Begsr ChkLine;

        setll caky pca01;
        reade caky pca01;

        dow not %eof;

            if carcn = eorcn;

              // delete the messages,eob and history from files
              exsr DelLine;

              // If its a new line added by claimcheck
              // ('Y' = New line added by claimcheck and payable
              //  'D' = New line added by claimcheck and denied  )

              if cacchk = 'Y' or cacchk = 'D';
                exsr DelNew;
              else;

           // Load original charges
                if caochg = 0;
                  caochg = cachrg;
                else;
           //*** Beg Add *** 10/09/12 **************************************
                  if cachrg = 0;
           //*** End Add *** 10/09/12 **************************************
                    cachrg = caochg;
           //*** Beg Add *** 10/09/12 **************************************
                  endif;
           //*** End Add *** 10/09/12 **************************************
                endif;

           // reset adjudication for existing denied lines
                if cacchk = 'N' and cacscd = '2';
                  cacscd = '1';
                endif;

                update pca01;
              endif;

            endif;

            reade caky pca01;

        enddo;

       EndSr;

       //*****************************************************************
       // DelNew - Delete new line messgaes/codes from all files         *
       //*****************************************************************
       BegSr DelLine;

        // delete from fdmgp
        chain mgky2 pmg01;
        if %found(fdmgl1);

         setgt mgky1 pmg01;
         readpe mgky2 pmg01;

         dow not %eof(fdmgl1);
           delete pmg01;
           readpe mgky2 pmg01;
         enddo;

        endif;

        // delete from fddep all except user entered.
        chain deky2 pde01;
        if %found(fddel);

         setgt deky1 pde01;
         readpe deky2 pde01;

         dow not %eof(fddel);
        //*** Beg Add *** 10/03/13 *************************************
           if deesrc <> 'U';
        //*** End Add *** 10/03/13 *************************************
             delete pde01;
        //*** Beg Add *** 10/03/13 *************************************
           endif;
        //*** End Add *** 10/03/13 *************************************
           readpe deky2 pde01;
         enddo;
        endif;

        // delete from history file
        chain khky pkh01;
         if %found(fdkhl2);
           delete pkh01;
         endif;

       EndSr;
       //*****************************************************************
       // DelNew - Delete new lines added by claimcheck from all files  *
       //*****************************************************************
       BegSr DelNew;

        if eobtyp ='N';
         chain dhky pdh01;
         if %found;
          delete pdh01;
         endif;

        elseif eobtyp = 'H';
         chain duky pdu01;
         if %found;
          delete pdu01;
         endif;
        endif;

        // delete from fddap
        chain daky pda01;
        if %found(fddal4);
         delete pda01;
        endif;

        //*** Beg Add *** 10/03/13 *************************************
        // delete from fdmgp
        chain mgky2 pmg01;
        if %found(fdmgl1);

         setgt mgky1 pmg01;
         readpe mgky2 pmg01;

         dow not %eof(fdmgl1);
           delete pmg01;
           readpe mgky2 pmg01;
         enddo;

        endif;

        // delete from fddep all except user entered.
        chain deky2 pde01;
        if %found(fddel);

         setgt deky1 pde01;
         readpe deky2 pde01;

         dow not %eof(fddel);
           delete pde01;
           readpe deky2 pde01;
         enddo;
        endif;
        //*** End Add *** 10/03/13 *************************************

        // delete from fdcap
                delete pca01;
       EndSr;

      /end-free
