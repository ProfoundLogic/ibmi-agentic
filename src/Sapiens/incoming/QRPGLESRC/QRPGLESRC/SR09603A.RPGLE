     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
    H/TITLE SR09603A - FL Endorsements WC090603A
     //===============================================================*
     // WC090603A                                                     *
     //                                                               *
     //   Determines if this endorsement form should be attached to   *
     //   the policy and creates data in the Endorsement Form         *
     //   Workfile (WXWXFMP) prior to a form merge.                   *
     //                                                               *
     // Created   04/29/24  C:79 Project: WC2969                      *
     //===============================================================*
     // REVISIONS:                                                    *
      //                                                               *
      // 08/21/25 C:333 Support: 148/117703  (Recompile)               *
      //                For single print endorsements provide specific *
      //                group and employer so that correct sig is found*
     //                                                               *
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
     FWTDDL2    If   E           K DISK
     //
     FWMNXL     If   E           K DISK
     fwtabl     If   e           k disk    include(pbs01)
     fwmldl1    If   e           k disk
     // Copybook F specs
     F/COPY SCOPYEN01F
    
    
     //=========================================================================
     //  Definition SpecsAA
     //=========================================================================

      // Entry
     D SR09603A        PR
     D                                3
     D                                3
     D                                9
     D                                5
     D                                3
     D                                1
     D                                9  0
     D                                1
     D SR09603A        PI
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
     D QQFORM          S             15A   inz('WC090603A ')

      //* Additional entry program
     D addpgm          c                   const('SRXXXXXXXX')
     d xxampo          s                   like(FMA701)
     d xxamag          s                   like(FMA702)
     d xxDedSize       s              1
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
    
     //=========================================================================
     //  OTHINF - Subroutine to load form specIfic information & write record
     //=========================================================================
       Begsr OthInf;

         Setll elky2 pel02;
         Reade elky2 pel02;
         Dow Not %Eof(wdell1);
            xxCxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv);
            If not(xxCxDate <= ELpfdt and xxCxDate <> *zeros);
               Chain rsky prs01;
               If %found(WMRSL) and (RSjrSt = Enst);
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
                  If Not %Eof(WTDDL2) and (DDamPo <> 0) and (DDstat = 'A');
                     FMa701 = DDamPo;
                     Exsr WriteWorkFile;
                     Leave;
                  Endif;

               Endif;

            Endif;

            Reade elky2 pel02;
         Enddo;

       Endsr;

       //=========================================================================
       //  PGMSTR - Beginning of program processing
       //=========================================================================
       Begsr PgmStr;
         WriteInCopyBook = *off;
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
         crtFrm = *on;
       Endsr;
