     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller) BndDir('WTBNDDIR')

      /TITLE SR20304B - MA - Massachusetts Employee Leasing endorsement
      //===============================================================*
      // WC200304B -  Massachusetts Employee Leasing endorsement       *
      //                                                               *
      // PROGRAM DESCRIPTION:                                          *
      //                                                               *
      //   Determines if this endorsement form should be attached to   *
      //   the policy and creates data in the Endorsement Form         *
      //   Workfile (WXFMP) prior to a form merge.                     *
      //===============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 10/06/17 C:062 Project: 150WAP                                *
      //                Remove peo stuff from valid                    *
      //                                                               *
      // 07/24/17 C:180 Project: 150WAP                                *
      //                Handle groups with combined PEOs.              *
      //                                                               *
      // 04/12/17 C:65  Project: WC2483                                *
      //                Populate WXPEP record with client data.        *
      //                Scrub the name and address fields for          *
      //                puncuation.                                    *
      //                                                               *
      // 02/28/17 C:291 Project: WC2481                                *
      //                Change to auto attach. Write all MA clients    *
      //                to schedule.                                   *
      //===============================================================*
      // NOTES:                                                        *
      //                                                               *
      //  Utilizes the endorsement copybook (SCOPYEN01F/D/C).          *
      //                                                               *
      //  qqmode = 1 - Called to determine if form should be attached. *
      //  qqmode = 2 - Called to print endorsement.                    *
      //  qqmode = 3 - Called for additional input.                    *
      //  qqmode = 4 - Called for midterm processing.                  *
      //===============================================================*
      /Copy sCopyEn01F
     f*** begin add ***  04/12/17  *************************************
     FWXPEL     UF A E           K DISK
     FWXPEL1    IF   E           K DISK    RENAME(PPE01:PPE02)
     f*** end add *****  04/12/17  *************************************
      /Copy sCopyEn01D
      /Copy wPrPeo
     d*** begin add ***  04/12/17  *************************************
     D/COPY SPRSTRING
      /Copy wWCPOLDAT4
     d xxunid          s                   like(PEUNID)
     d x1end#          s                   like(PEEND#)
     d xxFirst         s              1a
     d*** end add *****  04/12/17  *************************************

     D qqForm          s             15A   Inz('WC200304B ')
     D addPgm          c                   const('SRXXXXXXXX')

     D SR20304B        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1

     D SR20304B        PI
     D  qqCo#                         3
     D  qqFnd                         3
     D  qqEmp#                        9
     D  qqDiv                         5
     D  qqFyr                         3
     D  qqMode                        1
     D  qqRrn                         9  0
     D  qqMod2                        1

      /Copy sCopyEn01C
     C*** begin add ***  04/12/17  *************************************
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
     C*** end add *****  04/12/17  *************************************

      /Free
       //=========================================================================
       //  OTHINF - Subroutine to load form specific information
       //=========================================================================
       Begsr OthInf;
        //*** begin add ***  04/12/17  ****************************************
        xxFirst = ' ';
        //*** end add *****  04/12/17  ****************************************
        Setll (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        Dou %eof;
           Reade (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           Select;
           When %eof;
              Iter;
           EndSL;
           Chain (xxCo#:ELrtCd) prs01;
           If RSjrSt = Enst;
              dsCltInfo = GetCltInfo(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
              FM2002 = dsCltInfo.name;
              FMead1 = dsCltInfo.address1;
              FMead2 = dsCltInfo.address2;
              FMead3 = dsCltInfo.address3;
           //*** begin add ***  04/12/17  ****************************************
           //*
           //* Get latest UNID
           //*
              xxdt = *hival;
              Setgt pekey ppe02;
              Readpe(n) pekey1 ppe02;
              If not %eof;
                 xxunid = PEunid;
              Endif;

           //*
           //* Copy data into the record.
           //*
              x1end# = %trim(qqform);
              Chain(n) pekey2 ppe01;
              If %found;

                 If xxFirst = ' ';
                    Chain pekey2 ppe01;
                 Endif;

                 Clear e20304;

                 x7cname = %trim(ExtrAlphaNum(dsCltInfo.name));
                 x7cfein = dsCltInfo.fein;

                 If %subst(dsCltInfo.address2:1:5) <>
                  %subst(dsCltInfo.city:1:5);
                    x7cadr = %trim(ExtrAlphaNum(dsCltInfo.Address1)) + ' ' +
                     %trim(ExtrAlphaNum(dsCltInfo.Address2));
                 Else;
                    x7cadr = %trim(ExtrAlphaNum(dsCltInfo.Address1));
                 Endif;

                 x7ccity = %trim(ExtrAlphaNum(dsCltInfo.city));
                 x7cst   = dsCltInfo.state;
                 x7czip  = dsCltInfo.zip;

                 PEData = %trim(e20304);

                 PEName = %trim(ExtrAlphaNum(PEName));

                 If xxFirst = ' ';
                    Update ppe01;
                    xxFirst = 'N';
                 Else;
                    Write ppe01;
                 Endif;
              Endif;
           //*** end add *****  04/12/17  ****************************************

              Exsr WriteWorkFile;
           Endif;
        Enddo;
       Endsr;
       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr pgmstr;
        WriteInCopybook = *off;
       Endsr;
       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr pgmend;
       Endsr;
       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;
       //*** Begin Chg *** 10/06/17 *********************************************
       // Exsr getFnd;
       // Select;
       // When FNpeof = *blank;
       //    crtFrm = *Off;
       // When FNpeof = 'S' and ELdiv <> 0;
       //    crtFrm = *On;
       // When FNpeof = 'M' and ELemp# <> 1;
       //    crtFrm = *On;
       // //*** Begin Add *** 07/24/17 *******************************************
       // // Combined PEO Group - Master PEO client
       // When FNpeof = 'C' and ELemp# = 1 and ELdiv <> *Zero;
       //    Crtfrm = *on;
       //
       // // Combined PEO Group - MCP client
       // When FNpeof = 'C' and ELemp# <> 1;
       //    Crtfrm = *on;
       // //*** End   Add *** 07/24/17 *******************************************
       // Other;
       //    crtFrm = *off;
       // Endsl;
          crtFrm = *On;
       //*** End   Chg *** 10/06/17 *********************************************
       Endsr;
      /End-free
