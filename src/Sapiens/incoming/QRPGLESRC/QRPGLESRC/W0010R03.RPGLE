     H* (c) 2018 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
      *=========================================================================
     H/TITLE -  Group copy RPGILE program                                      *
      *                                                                        *
      *  DESCRIPTION: This Program wll call  CL program to verify and copy     *
      *  new group from existing group                                         *
      *                                                                        *
      *                                                                        *
      *   CALLED BY: W0010R02                                                  *
      *                                                                        *
      *   CALLS TO : W0010C03 ,F0060C03,W0010C02                               *
      *                                                                        *
      *  CREATED: 01/24/2008  C:189 PROJECT WC1621                             *
      *=========================================================================
      **************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H* 04/10/18 C:33   Project: 150WCF                                        *
     H*                 Make this program a pass-thru call                     *
     H*                                                                        *
     H* 11/18/13 C:65   Project: 129WSF                                        *
     H*                 Add next available employer/quote/case number entries  *
     H*                 to screen.  The next available employer/quote fields   *
     H*                 will be hidden if the group being copied is claims     *
     H*                 only.                                                  *
     H*                                                                        *
     H* 10/25/10 C:255 Project: WC1866                                         *
     H*                 Remove changes for Project WC1866 done on              *
     H*                 04/30/10.                                              *
     H*                                                                        *
     H* 04/30/10 C:255  Project: WC1866                                        *
     H*                 Group Specific Billing JOB Queues                      *
     H*                 Calls WA001R to determine company name and calls       *
     H*                 program W0010C02 to create Billing JOBQs               *
      ***************************************************************************
     H BNDDIR('WTBNDDIR')
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW0010F03  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSSYLF     if   e           k disk
     F*** begin add ***  11/18/13  ************************************************
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)

       //***  Begin Add  ***  04/10/18  ***********************************
     DWtGrpCopy        pr                  extPgm('WTGRPCPY')
     D outGrpFrom                     3  0
     D inRtCd                        10

     D inRtCd          s             10
       //***  Begin Add  ***  04/10/18  ***********************************

      /copy sPrDate
     F*** end add *****  11/18/13  ************************************************
      *=========================================================================
      *
     D PSDS          ESDS                  Extname(Szq1p)
      *
     C                   movel     'SY'          XXSYK
     C     SYKY          Chain     SSYLF
     C                   movel     SYFL12        XXFL12            1
     C                   If        XXFL12 <> 'Y'
      *  SHOW ERROR SCREEN
     C                   EXFMT     INSTERR
     C                   Eval      @@kl  = '1'
     C                   Eval      *Inlr = *On
     C                   return
     C                   endif                                                  If        XXFL12 <>
      *
     C*** begin add ***  11/18/13  ************************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C* Call API to retrieve CO#
     C                   Z-add     0             currDate          7 0
     C                   CALL      'WA001R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@CONM           40
     C                   PARM                    @@FNAM           40
      /free

       currDate = DateTo7(SystemDate);
       showEmplr = *on;

       Chain (@@CO#:@@FRGRP) PFN00;

       If %found;

          Setgt (@@CO#:@@FRGRP) PF100;
          Readpe (@@CO#:@@FRGRP) PF100;

          If not %eof;

             If F1CLMS = 'Y';
                showEmplr = *off;
             Endif;

          Endif;

       Endif;

      /end-free
     C*** end add *****  11/18/13  ************************************************
      *
     C                   Z-Add     @@FRGRP       Scfgrp
     C                   Dow       *in90 = *off
      *  SHOW GRUP CPY SCREEN
     C                   EXFMT     GRPCOPY
     C                   Eval      *in62  = *off
     C                   Eval      *in61  = *off
      *
     C                   If        sctgrp = 0  and *inkl = *off
     C                   Eval      *in62  = *on
     C                   Goto      JUMP
     C                   endif                                                            sctgrp = 0
     C*** begin add ***  11/18/13  ************************************************
      /free
       erNxtEmpN = *off;
       msNxtEmpN = ' ';
       erNxtQteN = *off;
       msNxtQteN = ' ';
       erNxtCaseN = *off;
       msNxtCaseN = ' ';

       If *inkl = *off;
          If cltid = '099';
          //* Next available case # must be entered.
             If scNxtCaseN <= 0;
                erNxtCaseN = *on;
                msNxtCaseN = 'WCR0502';
             Endif;
          Else;
          //* Next available case number cannot be less than zero.
             If scNxtCaseN < 0;
                erNxtCaseN = *on;
                msNxtCaseN = 'WCR0547';
             Endif;
          Endif;

          If cltid = '099' or showEmplr = *off;

          //* Next available employer/quote numbers cannot be less than zero.
             If scNxtEmpN < 0;
                erNxtEmpN = *on;
                msNxtEmpN = 'WCR0547';
             Endif;

             If scNxtQteN < 0;
                erNxtQteN = *on;
                msNxtQteN = 'WCR0547';
             Endif;
          Else;

          //* Next available employer/quote numbers must be entered.
             If scNxtEmpN <= 0;
                erNxtEmpN = *on;
                msNxtEmpN = 'WCR0502';
             Endif;

             If scNxtQteN <= 0;
                erNxtQteN = *on;
                msNxtQteN = 'WCR0502';
             Endif;

          Endif;

       Endif;

      /end-free
     C                   If        erNxtQteN = *on or erNxtEmpN = *on
     C                             or erNxtCaseN = *on
     C                   Goto      JUMP
     C                   Endif
     C*** end add *****  11/18/13  ************************************************
      *
     C                   movel     sctgrp        @tgrpc            3
     C                   movel     scfgrp        @fgrpc            3
     C                   movel     'VERIFY'      @mode            10
     C                   If        *Inkl = *On
     C                   Eval      @@kl  = '1'
     C                   Eval      *Inlr = *On
     C                   return
     C                   endif                                                  *Inkl = *On
      *
     C                   movel     'VERIFY'      @mode            10
     C
      *Call thr Program to check the WCMS group files in 'VERIFY' mode
      *
     C                   Call      'W0010C03'
     C                   Parm                    @mode
     C                   Parm                    @tgrpc
     C                   Parm                    @fgrpc
     C                   Parm                    @err              1
     C*** begin add ***  11/18/13  ********************************************
     C                   Parm      *blanks       @nextEmpN         9
     C                   Parm      *blanks       @nextQteN         9
     C                   Parm      *blanks       @nextCaseN        9
     C*** end add *****  11/18/13  ********************************************
      *
     C                   If        @err  = '  ' and ##MCIF = 'Y'
      *
      *Call thr Program to check the WCMC group files in 'VERIFY' mode
     C                   Call      'F0060C03'
     C                   Parm                    @mode
     C                   Parm                    @tgrpc
     C                   Parm                    @fgrpc
     C                   Parm                    @err
     C
     C                   endif                                                  @err  = '  '
      *
     C                   If        @err   <>  '  '
     C                   Eval      *in61         = *on
     C                   Goto      JUMP
     C                   else
     C                   Call      'WA008R'
     C                   Parm                    ##KL              1
     C
     C                   If        ##KL  = '0'
     C                   movel     *Blanks       @mode            10
     C                   movel     'COPY'        @mode            10
     C                   Eval      *in90         = *on
     C*** begin add ***  11/18/13  ************************************************
      /free

       //* Set Next available employer # for Copy Program.
       If scNxtEmpN = 0;
          @NextEmpN = '000000001';
       Else;
          @NextEmpN = %editc(scNxtEmpN:'X');
       Endif;

       //* Set Next available quote # for Copy Program.
       If scNxtQteN = 0;
          @NextQteN = '000500000';
       Else;
          @NextQteN = %editc(scNxtQteN:'X');
       Endif;

       //* Set Next available Case # for Copy Program.
       If scNxtCaseN = 0;
          @NextCaseN = '000000001';
       Else;
          @NextCaseN = %editc(scNxtCaseN:'X');
       Endif;

      /end-free
     C*** end add *****  11/18/13  ************************************************
      *
      *Call the Program to copy the WCMS group files in 'COPY'  mode
     C                   Call      'W0010C03'
     C                   Parm                    @mode
     C                   Parm                    @tgrpc
     C                   Parm                    @fgrpc
     C                   Parm                    @err
     C*** begin add ***  11/18/13  ************************************************
     C                   Parm                    @nextEmpN
     C                   Parm                    @nextQteN
     C                   Parm                    @nextCaseN
     C*** end add *****  11/18/13  ************************************************
     C
     C                   If        ##MCIF = 'Y'
     C*
      *Call the Program to copy  the WCMC group files in 'COPY'  mode
     C                   Call      'F0060C03'
     C                   Parm                    @mode
     C                   Parm                    @tgrpc
     C                   Parm                    @fgrpc
     C                   Parm                    @err
     C*
     C                   endif                                                  ##MCIF = 'Y'
     C*** BEG   ADD *****  04/30/10  ************************************
     C*** Beg   Del *****  10/25/10  ************************************
     C*If System control flag is Set.
     C*****              movel     'WC'          XXSYK
     C*****SYKY          Chain     SSYLF
     C*****              IF        WCFL51='Y'
     C* Call API to retrieve CO#
     C*****              CALL      'WA001R'
     C*****              PARM                    @@CO#             3 0
     C*****              PARM                    @@FND             3 0
     C*****              PARM                    @@CONM           40
     C*****              PARM                    @@FNAM           40
     C*Call program to create billing JOBQ for specific group.
     C*****              MOVE      @@CO#         ##CO#             3
     C*****
     C*****              CALL      'W0010C02'
     C*****              PARM                    ##CO#
     C*****              PARM                    @tgrpc
     C*****              PARM                    ERRFLG            1
     C*****
     C*****              ENDIF
     C*** End   Del *****  10/25/10  ************************************
     C*** END   ADD *****  04/30/10  ************************************
     C                   else
     C                   Eval      *in61         = *off
     C                   Eval      *in62         = *off
     C                   endif                                                  ##KL  = '0'
     C                   endif                                                  @err   <>  '  '
     C
     C     JUMP          Tag
     C                   Enddo
     C                   Eval      *Inlr = *On
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
     C     *Inzsr        Begsr
      *
     C     SYKY          Klist
     C                   Kfld                    XXSYK             2
      *
    C*
     C     *Entry        Plist
     C                   Parm                    @@FRGRP           3 0
     C                   Parm                    @@KC              1
     C                   Parm                    @@KL              1
      *
       //***  Begin Add  ***  04/10/18  ***********************************
      /Free
       WtGrpCopy(@@frGrp:inRtCd);
       *inlr = *on;
       Return;
      /End-free
       //***  End   Add  ***  04/10/18  ***********************************
     C
      *Call the Program to check for MANAGED CARE with ##MCIF
     C                   CALL      'SA001R'
     C                   Parm                    ##WCIF            1
     C                   Parm                    ##MCIF            1
     C                   Parm                    ##WCIN            1
     C                   Parm                    ##AR              1
     C                   Parm                    ##NTWK            1
     C                   Parm                    ##UR              1
     C                   Parm                    ##MED             1
     C                   Parm                    ##EDI             1
     C                   Parm                    ##WDCK            1
      *
      *
     C                   Endsr
