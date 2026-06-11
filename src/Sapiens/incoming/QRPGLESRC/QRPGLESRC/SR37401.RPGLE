     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BndDir('WTBNDDIR')
     H/TITLE SR37401 - PA Uncooperative Audit Endorsement
      //===============================================================*
      // Program Description:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //                                                               *
      // Created   : 09/06/16                                          *
      // Programmer: C:298                                             *
      // Project   : WC2070                                            *
      //===============================================================*
      // REVISIONS:                                                    *
      //                                                               *
      // 10/19/22 C:342 Project: WC2834                                *
      //                Endorsement Recognition of Cancellation        *
      //                                                               *
      // 05/09/17 C:180  Project: 93WLH/WC2481                       *
      //                 - Populate WXPEP record.                    *
      //
      //===============================================================*
      // Notes:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
      //                                                               *
      //  QQMODE = 1 - Called to determine if form should be attached. *
      //  QQMODE = 2 - Called to print endorsement.                    *
      //  QQMODE = 3 - Called for additional input.                    *
      //  QQMODE = 4 - Called for mid-term processing.                 *
      //===============================================================*
     fwdrrl     if   e           k disk
     f*** Begin Add ***  05/09/17  *************************************
     FWXPEL     UF a E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** End Add *****  05/09/17  *************************************

      //* Copybook F specs
     f/copy scopyen01f

     f*** Begin Add ***  05/09/17  *************************************
     D/COPY SPRSTRING
     D/Copy wWCPOLDAT4
      //***** Beg Del *** 10/19/22 **********************************
     D*****/Copy wPrEmplr
      //***** End Del *** 10/19/22 **********************************
     D xxunid          s                   like(PEUNID)
     D x1end#          s                   like(PEEND#)
     d xxRateSt        s              2a   inz
     f*** End Add *****  05/09/17  *************************************
     d qqForm          s             15a   inz('WC370401  ')

      //* Additional entry program
     d addPgm          c                   const('SRXXXXXXX ')

     d/copy scopyen01d

     D SR37401         pr
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR37401         pi
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1
    
      //=========================================================================
      //  Main
      //=========================================================================
    
      //* Copybook C specs (KLISTS and mainline)
     c/copy scopyen01c
     f*** Begin Add ***  05/09/17  *************************************
     C     PEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXDT              7 0

     C     PEKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR

     C     PEKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXUNID
     C                   KFLD                    X1END#
     f*** End Add *****  05/09/17  *************************************
    
      //=========================================================================
      //  OTHINF - Subroutine to load form specific information
      //=========================================================================
      /Free
       Begsr OthInf;
       // Begin add ***  05/09/17  ****************************************
          xxRateSt = GetRatingState(xxco#:xxfnd:xxemp#:xxdiv:Elfyr);

       // Get latest UNID
          xxdt = *hival;
          Setgt pekey ppe02;
          Readpe(n) pekey1 ppe02;
          If not %eof;
              xxunid = PEunid;
          Endif;

       // Copy data into the WCPOLS record.

          x1end# = %trim(qqform);
          Chain pekey2 ppe01;          //Chain for retrieval
          If %found;
             Clear e37401;
             x37ANCMult1  = *zeros;
             x37ANCMult2  = *zeros;
             x37ANCMult3  = *zeros;

             x37State1    = xxRateSt;
             x37ANCbasis1 = 'Estimated Annual Premium';
             x37ANCMult1  = 2000;
             PEData = e37401;
             PEName = %trim(ExtrAlphaNum(PEName));
             Update ppe01;
          Endif;

       // End   Add *** 05/09/17 ******************************
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
       RunningSrText = *On;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;

          Setgt  (ELco#:ELfnd:RSjrst:ELpfdt) prr01;
          Readpe (ELco#:ELfnd:RSjrst) prr01;
          If Not(%Eof) and RRancs <> 0 and RRancm <> 0;
             crtFrm = *on;
          Endif;

       Endsr;
      /End-Free
