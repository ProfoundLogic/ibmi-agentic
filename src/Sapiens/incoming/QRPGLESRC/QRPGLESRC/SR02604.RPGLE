     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
    H/TITLE SR02604 - AZ Benefits Deductible Endorsement
     //===============================================================*
     // WC020604                                                      *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXFMP) prior to a form merge.                     *
     //                                                               *
     // 11/17/22  C:340 Project: WC2886                               *
     //                                                               *
     //===============================================================*
     // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
     //===============================================================*
     // Notes:                                                        *
     //                                                               *
     //  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     //                                                               *
     //  QQMODE = 1 - Called to determine If form should be attached. *
     //  QQMODE = 2 - Called to print endorsement.                    *
     //  QQMODE = 3 - Called for additional input.                    *
     //  QQMODE = 4 - Called for mid-term processing.                 *
     //===============================================================*
     //
     //  Deductible Discount Code Table
     FWTDDL2    IF   E           K Disk
     FWMNXL     IF   E           K DISK
     fWTABL     IF   E           K Disk    include(pbs01)
     fWMLDL1    IF   E           K Disk
     fWXFML12   IF   E           K Disk    Rename(pfm01:pfm02)
     // Copybook F specs
     F/COPY SCOPYEN01F
    
     //=========================================================================
     //  Definition Specs
     //=========================================================================

      // Entry
     D SR02604         PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR02604         PI
     D  qqco#                         3
     D  qqfnd                         3
     D  qqemp#                        9
     D  qqdiv                         5
     D  qqfyr                         3
     D  qqmode                        1
     D  qqrrn                         9  0
     D  qqmod2                        1
      //* Copybook D specs
     D/COPY SCOPYEN01D


      //=========================================================================
      //  /COPY required constants
      //=========================================================================

      //* Form ID
     D QQFORM          S             15A   inz('WC020604  ')

      //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     d xxampo          s                   like(FMAM01)
     d xxamag          s                   like(FMAM01)
     d xxDedSize       s              1
     d isWriteMulti    s               n   Inz(*off)
     d strCity         s             25a
     d strSt           s              2a
     d strZip          s             10a
      //=========================================================================
      //  Prototypes
      //=========================================================================
     d rtvDedSize      pr             1                                         return S/L
     d  co#                                like(ELCO#)
     d  bscd                               like(ELBSCD)
     d rtvSmDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
     d rtvLgDedInfo    pr
     d  ampo                               like(FMAM01)
     d  amag                               like(FMAM01)
     d chkCancDate     pr                  Extpgm('SREFFD')
     d  inCo#                         3
     d  inFnd                         3
     d  inFyr                         3
     d  inEmp#                        9
     d  inDiv                         5
     d  outdate                       7
     d  inCo#          s              3
     d  inFnd          s              3
     d  inFyr          s              3
     d  inEmp#         s              9
     d  inDiv          s              5
     d  outdate        s              7
     d InCxDate        s              7  0

      //=========================================================================
      //  *LIKE DEFN definitions
      //=========================================================================


      //=========================================================================
      //  PLIST definitions
      //=========================================================================

      //* Entry parameters

      //* Additional input program parameters


      //=========================================================================
      //  KLIST definitions
      //=========================================================================
      //
      //  DEDUCTIBLE DISCOUNT CODE FILE
      //
     C     DDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C                   KFLD                    XXDDT             7 0
      //
     C     DDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELDCDE
     C     NXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
      //
     C     NXKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    RSJRST
    
     //=========================================================================
     //  Main
     //=========================================================================
    
     //* Copybook C specs
     C/COPY SCOPYEN01C
     //=========================================================================
     //  OTHINF - Subroutine to load form specIfic information & write record
     //=========================================================================
       Begsr OthInf;

         Exsr OpenWorkfile;

         Setll elky2 pel02;
         Reade elky2 pel02;
         Dow Not %Eof(wdell1);
              InCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
            If not(InCxDate <= ELpfdt and InCxDate <> *zeros);
               Chain rsky prs01;
               If %found(WMRSL) and (RSjrSt = Enst);
                  Setll NXky1 PNX01;
                  ReadPe NXky2 PNX01;
                  If not %Eof(WMNXL);
                     FMa702 = NXinsrCxDy;
                     FMa703 = NXinsdCxDy;
                     Leave;
                  Endif;
               EndIf;
            Endif;
            Reade elky2 pel02;
         Enddo;

         i = 1;
         If ENmnAu = 'M';
            Setll (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
            Reade (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
            Dow not %eof(wxfml12);
               FM2001 = F3500A1;  // Person or Organization
               FM2002 = F3500A2;   // Email
               FMeAd1 = F3eAd1;
               FMeAd2 = F3eAd2;
               If F3c301 = *blanks;
                  strCity = '0';
               Else;
                  strCity = F3c301;
               Endif;
               If F3emSt = *blanks;
                  strSt = '0';
               Else;
                  strSt = F3emSt;
               Endif;
               If F3am01 <> 0;
                  strZip = %Char(F3am01*100);
                  strZip = %Subst(strZip:1:5) + '-' + %Subst(strZip:6:4);
               Else;
                  strZip = '0';
               Endif;
               FMeAd3 = %Trim(strCity) + ',' + %Trim(strSt) + ',' +
                %Trim(strZip);
               Exsr WriteMultiRcd;
               isWriteMulti = *on;
               Reade (xxCo#:xxFnd:xxEmp#:xxFyr:xxForm) pfm02;
            Enddo;
         Endif;

         If Not isWriteMulti;
            Exsr WriteMultiRcd;
         Endif;

         Exsr CloseWorkFile;

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
         WriteInCopybook = *off;
       Endsr;

       //=========================================================================
       //  PGMEND - End of program processing
       //=========================================================================
       Begsr PgmEnd;
         //
       Endsr;

       //=========================================================================
       //  VALID - Subroutine to attach this form to the policy
       //=========================================================================
       Begsr Valid;

         Chain rsky prs01;
         If %Found(WMRSL) and RSjrSt = Enst;

            //  RETRIEVE THE DEDUCTIBLE RECORD
            inCo#   = %Editc(ELCo#:'X');
            inFnd   = %Editc(ELfnd:'X');
            inFyr   = %Editc(ELfyr:'X');
            inEmp#  = %Editc(ELemp#:'X');
            inDiv   = %Editc(ELdiv:'X');
            outDate = %Editc(ELdcEf:'X');

            Callp chkCancDate(inCo#:inFnd:inFyr:inEmp#:inDiv:outDate);

            xxDdt = %Dec(outdate:7:0);

            Setgt ddky pdd01;
            Readpe ddky1 pdd01;
            //
            //  If A RECORD IS FOUND, AND THERE IS A PER OCCURRENCE DEDUCTIBLE,
            //  THEN PRINT THE ENDORSEMENT FORM
            //
            If Not %Eof(WTDDL2) and (DDAMPO <> 0) and (DDSTAT = 'A');
            //
            //* Found a division with a matching jurisdiction state
               crtFrm = *on;
            Endif;
         Endif;

       Endsr;
