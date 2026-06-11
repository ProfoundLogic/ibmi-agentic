     H* (c) 2011 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
      *=========================================================================
     H/TITLE - Program to Determine billing JOBQ                               *
      *                                                                        *
      *  DESCRIPTION: This Program will determine billing JOBQ. It will check  *
      *  whether client is Data Center client, based on that it will determine *
      *  Alternate JOBQ name.                                                  *
      *                                                                        *
      *   CALLED BY: W7710C, W8001C                                            *
      *                                                                        *
      *   CALLS TO : SA030R                                                    *
      *                                                                        *
      *  CREATED: 04/30/10    C:255 PROJECT WC1866                             *
      *=========================================================================
      **************************************************************************
      * REVISIONS:                                                             *
      *
      * 08/01/11 C:190 Project: 133WLP                                         *
      *                Modified program to also check new policy page JOBQ     *
      *
      * 01/24/11 C:061 WC1866 prtsk 47
      *                Change to use WCFL47 instead of WCFL51.
      *
      * 10/25/10 C:255 Project: WC1866                                         *
      *                Retrieve JobQ name from Group Auxiliary file instead of *
      *                creating in program.                                    *
      *                                                                        *
      ***************************************************************************
     FSSYLF     if   e           k disk
     FSMCOL     IF   E           K Disk
     F*** beg add ***** 10/25/10 ******************************************
     FWDF2L     IF   E           K Disk
     F*** end add ***** 10/25/10 ******************************************
      *=========================================================================
     D@AltJobQ         S             10A
     D@OBJTYPE         S              7A   INZ('*JOBQ')
     D*** beg add *** 08/01/11 *********************************************
     D PolCall         s               n
     D ##pgm           s             10a
     D ##found         s              1a
     D*** end add *** 08/01/11 *********************************************
     C* Read System control file
     C                   movel     'WC'          XXSYK
     C     SYKY          Chain     SSYLF
     C* Set the pointer to read Company level flag
     C                   Move      @@CO#         ##CO#             3 0
     C     ##CO#         Chain     SMCOL
     C*
     C                   Move      @@CO#         CO#               3
     C                   Move      @@FND         FND               3
     C*** beg add ***** 10/25/10 ******************************************
     C                   Move      @@FND         ##FND#            3 0
     C*** end add ***** 10/25/10 ******************************************
     C                   Move      @@FYR         FYR               3
     C                   Move      @@EMP         EMP               9
     C*
     C*** beg add *** 08/01/11 *********************************************
     C* Check to see where this program was called.
     C*
     C*
     C                   eval      polCall=*off
     C                   call      'SRCHKPGM'                           99
     C                   parm      'W7412C'      ##pgm
     C                   parm      *blanks       ##found
     C*
     C                   if        ##found = *on
     C                   eval      PolCall = *on
     c                   else
     C                   call      'SRCHKPGM'                           99
     C                   parm      'W7415C'      ##pgm
     C                   parm      *blanks       ##found
     C                   if        ##found = *on
     C                   eval      PolCall = *on
     c                   endif
     C                   endif
     C*** end add *** 08/01/11 *********************************************
     C                   Call      'SA030R'
     C                   Parm                    suffix            3
     C                   Parm                    pflr             80
     C*** beg add ***** 10/25/10 ******************************************
     C*Chain Group Auxiliary file
     C     f2ky          chain     pf201
     C*** end add ***** 10/25/10 ******************************************
     C*Determine Alternate JOBQ Name for Data Center and non-data center clients
     C                   If        pflr <> *blanks
     C*
     C* Data Center client Alt JOBQ e.g. PDSJOBB132. Group JOBQ e.g. P132JOBfff
     C                   Eval      @AltJobQ='PDSJOBB' + suffix
     C                   Eval      @GrpJobQ='P' + suffix + 'JOB'
     C*** beg add *** 08/01/11 *********************************************
     C                   If        polcall = *on and copflg = 'Y'
     C                             and @@emp = *zeros
     C                   Eval      @AltJobQ='PDSJOBP' + suffix
     c                   endif
     C*** end add *** 08/01/11 *********************************************
     C*
     C                   Else
     C*
     C* Non-data Center client Alt JOBQ e.g. QBATCHB132. Group JOBQ e.g. QBATCHfff
     C                   Eval      @AltJobQ='QBATCHB'
     C                   Eval      @GrpJobQ='QBATCH'
     C*** beg add *** 08/01/11 *********************************************
     C                   If        polcall = *on and copflg = 'Y'
     C                             and @@emp = *zeros
     C                   Eval      @AltJobQ='QBATCHP'
     c                   endif
     C*** end add *** 08/01/11 *********************************************
     C*
     C                   EndIf                                                  pflr <> *Blanks
     C*
     C*(Check for single policy) AND (Policy in Quote)
     C                   If        @@QTE='Q'
     C*For single policy
     C                   IF        @@FYR<>*Zeros AND @@EMP<>*Zeros
     C*Program to Determine New/Renew status in Quote
     C                   CALL      'WA069C'
     C                   PARM                    CO#
     C                   PARM                    FND
     C                   PARM                    FYR
     C                   PARM                    EMP
     C                   PARM                    NRW               1
     C*
     C                   If        NRW='R'
     C*
     C                   EXSR      SRProcess
     C*If policy is New
     C                   Else
     C*** beg chg *** 08/01/11 *********************************************
     C**********         If        COBFLG='Y'
     C                   If        cobflg='Y' or copflg='Y'
     C*** end chg *** 08/01/11 *********************************************
     C                   Eval      @@JOBQ = @AltJobQ
     C                   Else
     C                   Eval      @@JOBQ = *BLANKS
     C                   EndIf
     C*
     C                   EndIf                                                   Renew Flag
     C*
     C                   ELSE
     C* FOR MASS BILLING
     C* Mass Processing for Renewal Business or Both
     C                   If        @@RNB='R' or @@RNB = 'B'
     C                   EXSR      SRProcess
     C*
     C* Mass Processing for New Business
     C                   else
     C*** beg chg *** 08/01/11 *********************************************
     C***********        If        COBFLG='Y'
     C                   If        cobflg='Y' or copflg='Y'
     C*** end chg *** 08/01/11 *********************************************
     C                   Eval      @@JOBQ = @AltJobQ
     C                   Else
     C                   Eval      @@JOBQ = *BLANKS
     C                   EndIf
     C*
     C                   Endif                                                   Renew Flag
     C
     C                   EndIf                                                   Single Policy
     C
     C*(Check for Mass Process) OR (Policy not in Quote)
     C                   Else
     C*
     C                   ExSr      SRProcess
     C*
     C                   EndIf                                                  Qte & single Policy
     C*
     C*End of Processing
     C                   Eval      *Inlr=*On
      *****************************************************************
     C     SRProcess     BegSr
     C*
     C* Initialize JOBQ to Alt JOBQ or Blanks
     C*** beg chg *** 08/01/11 *********************************************
     C***********        If        COBFLG='Y'
     C                   If        cobflg='Y' or copflg='Y'
     C*** end chg *** 08/01/11 *********************************************
     C                   Eval      @@JOBQ = @AltJobQ
     C                   Else
     C                   Eval      @@JOBQ = *BLANKS
     C                   EndIf                                                  COBFLG='Y'
     C*
     C*Check system flag for group specific job billing job queue
      ****** beg chg ****** 01/24/11 *************************************
     C******             If        WCFL51 = 'Y'
     C                   If        WCFL47 = 'Y'
      ****** end chg ****** 01/24/11 *************************************
     C*** beg chg ***** 10/25/10 ******************************************
     C********           Eval      @GrpJobQ=%trim(@GrpJobQ) + FND
     C                   eval      @GrpJobQ=F2JOBQ
     C*** end chg ***** 10/25/10 ******************************************
     C*
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    @GRPJOBQ         10
     C                   PARM                    @OBJTYPE
     C                   PARM                    @FLG              1
     C*
     C*If JOBQ for Group found
     C                   if        @Flg='0'
     C                   Eval      @@JOBQ=@GRPJOBQ
     C                   endif                                                  @Flg='0'
     C                   endIf                                                   Grp Q Ctrl flg
     C*
     C                   EndSr
      *****************************************************************
      *****************************************************************
      *  INITIALIZATION                ** SUBROUTINE **   S R 1 S T   *
      *****************************************************************
     C     *Inzsr        Begsr
    C*
     C     SYKY          Klist
     C                   Kfld                    XXSYK             2
    C*
     C*** beg add ***** 10/25/10 ******************************************
     C     F2KY          Klist
     C                   Kfld                    ##CO#
     C                   Kfld                    ##FND#
     C*** end add ***** 10/25/10 ******************************************
     C     *Entry        Plist
     C                   Parm                    @@CO#             3
     C                   Parm                    @@FND             3
     C                   Parm                    @@FYR             3
     C                   Parm                    @@EMP             9
     C                   Parm                    @@DIV             5
     C                   Parm                    @@QTE             1
     C                   Parm                    @@RNB             1
     C                   Parm                    @@JOBQ           10
     C*
     C                   Endsr
