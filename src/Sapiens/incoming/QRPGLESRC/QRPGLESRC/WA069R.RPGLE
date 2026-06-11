     HCopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     H dftactgrp(*no) actgrp(*caller)
      *** Beg Add ***** 06/17/24 *******************************************
     H BndDir('WTBNDDIR')
      *** End Add ***** 06/17/24 *******************************************
     H**********************************************************************
     H/TITLE WA069R - Determine the new/renewal flag
     H**********************************************************************
     H*  Description:  This program determines what the new/renewal        *
     H*                flag is for a given policy.                         *
     H*                                                                    *
     H*  Called from:                                                      *
     H*    W0051R02 - Policy Year Master Information                       *
     H*                                                                    *
     H*  CALLS TO:                                                         *
     H*    None                                                            *
     H*                                                                    *
     H**********************************************************************
     H*  Created on 09/04/2008 by c:62 for Project WC1611                  *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H* 03/28/25 C:65  Support: 113240 (AmWins)                            *
     H*                If a policy moved to a new group with same carrier  *
     H*                ID, check if old employer was renewal policy or not.*
     H*                                                                    *
     H* 06/17/24 C:333 Support: 115750 (AmWins)                            *
     H*                Group 95 - Prior Policy Number                      *
     H*                -Changes to check StateRepoting division of previous*
     H*                 year.                                              *
     H*                                                                    *
     H* 02/05/19 C:300 Support: PHW75643 (FMIC)                            *
     H*                Prior Policy# Not coming on Policy page             *
     H*                Change condition to return RPCAP# from WMRPP file   *
     H*                                                                    *
     H* 07/11/18 C:65  Project: 148WAY/Phw#71499                           *
     H*                Update to return the prior policy # in optional     *
     H*                parameter for the policy page process.              *
     H*                                                                    *
     H* 05/17/17 C:180 Project: C133WQD                                    *
     H*                Omit processing if renewed to Quote.                *
     H*                                                                    *
     H* 05/04/17 C:180 Project: C133WQD                                    *
     H*                Add new x-ref renewal flag.                         *
     H*                                                                    *
     H* 02/27/08 C:136 Support: WC1611                                     *
     H*                Enhance the efficiency of this program by reading   *
     H*                WDELL8 which sorts by co/fnd/emp/yr instead of      *
     H*                co/fnd/emp/div/yr.                                  *
     H**********************************************************************
      *** beg chg ***** 02/27/08 *******************************************
     F********* WDELL     IF   E           K DISK
     FWDELL8    IF   E           K DISK    rename(pel01:pel08)
      *** end chg ***** 02/27/08 *******************************************
     FWDF2L     IF   E           K DISK
     FWZEXL     IF   E           K DISK
     F*
     F*** begin add ***  07/11/18  *****************************************
     Fwmrpl     if   e           k disk    rename(prp01:prp00)
     D                 ds
     D xppol                         18
     D  xqfnd                         3    overlay(xppol)
     D  xqemp#                        9    overlay(xppol:4)
     D  xqfyr                         3    overlay(xppol:13)
     D psds          esds                  extname(szq1p)
     D*** end add *****  07/11/18  *****************************************
     D @@co#           s                   like(elco#)
     D @@fnd           s                   like(elfnd)
     D @@fyr           s                   like(elfyr)
     D @@emp#          s                   like(elemp#)
     D @@nwrw          s                   like(elnwrw)
     D xccar#          s                   like(f2car#)
     D xocar#          s                   like(f2car#)
     D*** Beg Add *****  06/17/24  *****************************************
     D xxDiv           s                   like(ELdiv)

      * copy books
      /copy wPrEmplr
     D*** End Add *****  06/17/24  *****************************************
     C*
     C     ELKYPA        KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C*
     C     ELKYEX        KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
     C                   KFLD                    EXOEMP
     C*
     C     F2KY          KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXFND
     C*
     C     F2KYEX        KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@co#
     C                   PARM                    @@fnd
     C                   PARM                    @@emp#
     C                   PARM                    @@fyr                          wdelp
     C                   PARM                    @@nwrw                         wdelp
     c*** begin add ***  07/11/18  *****************************************
     c                   Parm                    @@ppol           18
     C*
     C                   Clear                   xppol
     c*** end add *****  07/11/18  *****************************************
     C*
     c                   eval      @@nwrw='N'
     C*
     C* Let's check the employer detail for previous years
     C*
      *** beg chg ***** 02/27/08 *******************************************
     C*****elkypa        chain(n)  pel01
     C     elkypa        chain(n)  pel08
      *** end chg ***** 02/27/08 *******************************************
     C                   if        %found
      *** beg del ***** 02/27/08 *******************************************
     C***********        dou       %eof
      *** end del ***** 02/27/08 *******************************************
     c*
     c                   if        elfyr<@@fyr
     c                   eval      @@nwrw='R'
     c*** begin add ***  07/11/18  *****************************************
      /free
       If Q1prms >= 6;
          Setll (@@Co#:@@Fnd:@@Emp#:@@Fyr) pel08;
          Readpe (@@Co#:@@Fnd:@@Emp#) pel08;

          If not %eof;
             //*** Beg Chg ***** 06/17/24 *************************************
             //***Chain (@@Co#:@@Fnd:@@Emp#:ELDiv:ELFyr) prp00;
             xxDiv = GetStRptDv(ELCo#:ELFnd:ELEmp#);
             Chain (@@Co#:@@Fnd:@@Emp#:xxDiv:ELFyr) prp00;
             //*** End Chg ***** 06/17/24 *************************************
       //*** beg chg ***** 02/05/19 *******************************************
       //*** If %found;
             If %found and RPCap# <> *Blanks;
       //*** end chg ***** 02/05/19 *******************************************
                xppol = RPCap#;
             Else;
                xqfnd = %editc(ELFnd:'X');
                xqemp# = %editc(ELEmp#:'X');
                xqfyr = %editc(ELFyr:'X');
             Endif;
          Endif;
       Endif;
      /end-free
     c*** end add *****  07/11/18  *****************************************
      *** beg chg ***** 02/27/08 *******************************************
     c*****************  leave
     c                   else
     c                   eval      @@nwrw='N'
      *** end chg ***** 02/27/08 *******************************************
     c                   endif
     C*
      *** beg del ***** 02/27/08 *******************************************
     c*****elkypa        reade(n)  pel01
     c***********        enddo
      *** end del ***** 02/27/08 *******************************************
      *** Beg Add ***** 05/17/17 *******************************************
      /Free
       If ELrqfl = 'Y';
          @@nwrw = 'R';
       //*** begin add ***  07/11/18  *****************************************
          If Q1prms >= 6 and xppol = ' ';
             Setll (@@Co#:@@Fnd:@@Emp#:@@Fyr) pel08;
             Readpe (@@Co#:@@Fnd:@@Emp#) pel08;

             If not %eof;
             //*** Beg Chg ***** 06/17/24 *************************************
             //***Chain (@@Co#:@@Fnd:@@Emp#:ELDiv:ELFyr) prp00;
             xxDiv = GetStRptDv(ELCo#:ELFnd:ELEmp#);
             Chain (@@Co#:@@Fnd:@@Emp#:xxDiv:ELFyr) prp00;
             //*** End Chg ***** 06/17/24 *************************************
       //*** beg chg ***** 02/05/19 *******************************************
       //****** If %found;
                If %found and RPCap# <> *Blanks;
       //*** end chg ***** 02/05/19 *******************************************
                   xppol = RPCap#;
                Else;
                   xqfnd = %editc(ELFnd:'X');
                   xqemp# = %editc(ELEmp#:'X');
                   xqfyr = %editc(ELFyr:'X');
                Endif;
             Endif;
          Endif;
       //*** end add *****  07/11/18  *****************************************
       Endif;
      /End-Free
      *** End Add ***** 05/17/17 *******************************************
     c                   endif
     c*
     c* Let's check the employer x-ref file if this policy year has yet to
     c* be determined to be a renewal.
     c*
     c                   if        @@nwrw='N'
     c     elkypa        chain     pex01
     c                   if        %found
     c                   dou       %eof
     c*
     C*** begin del ***  03/28/25  ******************************************
     c*********          if        exfnd <> exofnd
     C*** end del *****  03/28/25  ******************************************
     c*
     c                   clear                   xccar#
     c                   clear                   xocar#
     c*
     C     f2ky          chain     pf201
     c                   if        %found
     c                   eval      xccar# = f2car#
     c                   endif
     c*
     C     f2kyex        chain     pf201
     c                   if        %found
     c                   eval      xocar# = f2car#
     c                   endif
     c*
      **** Begin Change *** 05/04/17 ******************************
     c****                   if        xccar#=xocar#
     C*** begin chg ***  03/28/25  ******************************************
     c********           if        xccar#=xocar# or EXrnwl = 'Y'
     c                   If        EXrnwl = 'Y'
     C*** end chg *****  03/28/25  ******************************************
      **** End   Change *** 05/04/17 ******************************
     c                   eval      @@nwrw='R'
     c*** begin add ***  07/11/18  *****************************************
      /free
       If Q1prms >= 6;
          Setgt (@@Co#:EXOFnd:EXOEmp) pel08;
          Readpe (@@Co#:EXOFnd:EXOEmp) pel08;

          If not %eof;
             //*** Beg Chg ***** 06/17/24 *************************************
             //***Chain (@@Co#:@@Fnd:@@Emp#:ELDiv:ELFyr) prp00;
             xxDiv = GetStRptDv(ELCo#:ELFnd:ELEmp#);
             Chain (@@Co#:@@Fnd:@@Emp#:xxDiv:ELFyr) prp00;
             //*** End Chg ***** 06/17/24 *************************************
       //*** beg chg ***** 02/05/19 *******************************************
       //*** If %found;
             If %found and RPCap# <> *Blanks;
       //*** end chg ***** 02/05/19 *******************************************
                xppol = RPCap#;
             Else;
                xqfnd = %editc(ELFnd:'X');
                xqemp# = %editc(ELEmp#:'X');
                xqfyr = %editc(ELFyr:'X');
             Endif;
          Endif;
       Endif;
      /end-free
     c*** end add *****  07/11/18  *****************************************
     C*** begin del ***  03/28/25  ******************************************
     c********           endif
     C*** end del *****  03/28/25  ******************************************
     c*
     c                   else
     C*** begin add ***  03/28/25  ******************************************
     c                   If        xccar#=xocar#
     C*** end add *****  03/28/25  ******************************************
     c*
      *** beg chg ***** 02/27/08 *******************************************
     C*****elkyex        chain(n)  pel01
     C     elkyex        chain(n)  pel08
      *** end chg ***** 02/27/08 *******************************************
     C                   if        %found
      *** beg del ***** 02/27/08 *******************************************
     C***********        dou       %eof
      *** end del ***** 02/27/08 *******************************************
     c*
     c                   if        elfyr<@@fyr
     c                   eval      @@nwrw='R'
     c*** begin add ***  07/11/18  *****************************************
      /free
       If Q1prms >= 6;
          Setgt (@@Co#:EXOFnd:EXOEmp) pel08;
          Readpe (@@Co#:EXOFnd:EXOEmp) pel08;

          If not %eof;
             //*** Beg Chg ***** 06/17/24 *************************************
             //***Chain (@@Co#:@@Fnd:@@Emp#:ELDiv:ELFyr) prp00;
             xxDiv = GetStRptDv(ELCo#:ELFnd:ELEmp#);
             Chain (@@Co#:@@Fnd:@@Emp#:xxDiv:ELFyr) prp00;
             //*** End Chg ***** 06/17/24 *************************************
       //*** beg chg ***** 02/05/19 *******************************************
       //*** If %found;
             If %found and RPCap# <> *Blanks;
       //*** end chg ***** 02/05/19 *******************************************
                xppol = RPCap#;
             Else;
                xqfnd = %editc(ELFnd:'X');
                xqemp# = %editc(ELEmp#:'X');
                xqfyr = %editc(ELFyr:'X');
             Endif;
          Endif;
       Endif;
      /end-free
     c*** end add *****  07/11/18  *****************************************
      *** beg chg ***** 02/27/08 *******************************************
     c***********        leave
     c                   else
     c                   eval      @@nwrw='N'
      *** end chg ***** 02/27/08 *******************************************
     c                   endif
     C*
      *** beg del ***** 02/27/08 *******************************************
     c*****elkyex******  reade(n)  pel01
     c*****************  enddo
      *** end del ***** 02/27/08 *******************************************
     c                   endif
     C*** begin add ***  03/28/25  ******************************************
     c                   Else
     c                   Eval      @@nwrw='N'
     c                   Endif
     C*** end add *****  03/28/25  ******************************************
     C*
     c                   endif
     C*
     c                   if        @@nwrw='R'
     c                   leave
     c                   endif
     C*
     c     elkypa        reade     pex01
     c                   enddo
     c                   endif
     c                   endif
     c*
     c*** begin add ***  07/11/18  *****************************************
     c                   If        Q1prms >= 6
     c                   Eval      @@ppol = xppol
     c                   Endif
     c*** end add *****  07/11/18  *****************************************
     c                   seton                                        lr
